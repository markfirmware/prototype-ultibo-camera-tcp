program Device;
{$mode delphi}{$H+}

uses
 RaspberryPi3,
 GlobalConfig,GlobalConst,GlobalTypes,
 Platform,Threads,SysUtils,Classes,FrameBuffer,Console,
 Camera,Ultibo,GraphicsConsole,
 Network,SMSC95XX,Winsock2,
 ImagingDeviceUnit,
 Keyboard;

var
 Console1,Console2,CameraConsole:TWindowHandle;
 CaptureLoopThreadHandle:TThreadHandle = INVALID_HANDLE_VALUE;
 LocalRenderLoopThreadHandle:TThreadHandle = INVALID_HANDLE_VALUE;
 KeyboardLoopThreadHandle:TThreadHandle = INVALID_HANDLE_VALUE;

procedure Log(S:String);
begin
 ConsoleWindowWriteLn(Console1,S);
end;

function WaitForIPComplete : string;
var
  TCP : TWinsock2TCPClient;
begin
  TCP := TWinsock2TCPClient.Create;
  Result := TCP.LocalAddress;
  if (Result = '') or (Result = '0.0.0.0') or (Result = '255.255.255.255') then
    begin
      while (Result = '') or (Result = '0.0.0.0') or (Result = '255.255.255.255') do
        begin
          sleep (1000);
          Result := TCP.LocalAddress;
        end;
    end;
  TCP.Free;
end;

procedure RestoreBootFile(Prefix,FileName:String);
var
 Source:String;
begin
 if BoardGetType <> BOARD_TYPE_QEMUVPB then
  begin
   Source:=Prefix + '-' + FileName;
   Log(Format('Restoring from %s ...',[Source]));
   while not DirectoryExists('C:\') do
    Sleep(500);
   if FileExists(Source) then
    CopyFile(PChar(Source),PChar(FileName),False);
   Log(Format('Restoring from %s done',[Source]));
  end;
end;

const
 FrameRate=10;
 Every=1;
 NumberOfImages=10*FrameRate;
 FrameWidth=160;
 FrameHeight=160;
 ImageSize=FrameWidth*FrameHeight;
 YPlaneOffset=0;

type
 TImageBuffer=Array[0 .. ImageSize - 1] of Byte;
 TImageQueue=Array[0 .. NumberOfImages - 1] of TImageBuffer;
 TListener = class(TWinsock2TcpListener)
 protected
  function DoExecute(AThread:TWinsock2TcpServerThread):Boolean; override;
 end;

var
 Listener:TListener;
 StatusCode:Cardinal;
 CameraBuffer:Pointer;
 Next:Integer;
 ImageQueue:TImageQueue;
 StartedWriting:Integer;
 FinishedWriting:Integer;
 ImagesFinishedReading:Integer;

procedure Check(Message:String;X:Boolean);
begin
 if not X then
  raise Exception.Create(Message);
end;

procedure TransmitFrame(AThread:TWinsock2TcpServerThread);
var
 ImageBuffer:PByte;
 ImageNumber:Integer;
 Freshest,NewFreshest:Integer;
 ImageMessage:TImagingMessage;
begin
 ImageNumber:=FinishedWriting;
 Freshest:=StartedWriting;
 Log(Format('image number %d being transmitted',[ImageNumber]));
 ImageBuffer:=@ImageQueue[ImageNumber mod NumberOfImages];
 ImageMessage.Kind:=MessageKindImage;
 with ImageMessage.Image do
  begin
   DataLength:=Imagesize;
   FrameNumber:=ImageNumber;
   LengthX:=FrameWidth;
   LengthY:=FrameHeight;
  end;
 Check('WriteData image message',AThread.Server.WriteData(@ImageMessage,SizeOf(ImageMessage)));
 Check('WriteData image data',AThread.Server.WriteData(Imagebuffer,ImageSize));
 NewFreshest:=StartedWriting;
 if NewFreshest >= ImageNumber + NumberOfImages then
  Log(Format('image number %d was overwritten during transmission ... started %d ... backlog %d ***********',[ImageNumber,NewFreshest,NewFreshest - ImageNumber]));
 ImagesFinishedReading:=ImageNumber;
end;

procedure LocalRender;
var
 PixelX,PixelY,PixelColor:Integer;
 ImageBuffer:PByte;
 ImageNumber:Integer;
 Freshest,NewFreshest:Integer;
begin
 ImageNumber:=FinishedWriting;
 Freshest:=StartedWriting;
 Log(Format('image number %d being rendered',[ImageNumber]));
 ImageBuffer:=@ImageQueue[ImageNumber mod NumberOfImages];
 for PixelX:=0 to FrameWidth - 1 do
  for PixelY:=0 to FrameHeight - 1 do
   begin
    PixelColor:=PByte(ImageBuffer + YPlaneOffset + PixelY*FrameWidth + PixelX)^;
    PixelColor:=PixelColor or (PixelColor shl 8) or (PixelColor shl 16) or (PixelColor shl 24);
    GraphicsWindowDrawPixel(CameraConsole,PixelX,PixelY,PixelColor);
   end;
 NewFreshest:=StartedWriting;
 if NewFreshest >= ImageNumber + NumberOfImages then
  Log(Format('image number %d was overwritten ... started %d ... backlog %d *******',[ImageNumber,NewFreshest,NewFreshest - ImageNumber]));
 ImagesFinishedReading:=ImageNumber;
end;

function KeyboardLoopThread(Parameter:Pointer):PtrInt;
var
 KeyboardChar: Char;
begin
 Result:=0;
 while True do
  begin
   ConsoleGetKey(KeyboardChar,Nil);
   if KeyboardChar = #27 then
    SystemRestart(0);
  end;
end;

function LocalRenderLoopThread(Parameter:Pointer):PtrInt;
begin
 Result:=0;
 ImagesFinishedReading:=0;
 while True do
  begin
   if FinishedWriting <> ImagesFinishedReading then
    LocalRender;
   ThreadSleep(10);
  end;
end;

procedure ServerStart;
var
 WSAData:TWSAData;
begin
 try
  FillChar(WSAData,SizeOf(TWSAData),0);
  if WSAStartup(WINSOCK_VERSION,WSAData) = ERROR_SUCCESS then
   begin
    Listener:=TListener.Create;
    Listener.BoundPort:=27000;
    Listener.Active:=True;
    Log('server started');
   end;
 except on E:Exception do
  begin
   Log(Format('ImagingTestProgram Exception.Message %s',[E.Message]));
    Sleep(5*1000);
  end;
 end;
end;

function TListener.DoExecute(AThread:TWinsock2TcpServerThread):Boolean;
var
 ImagingRequest,ImagingResponse:TImagingMessage;
 ClockMessage:TImagingMessage;
 Seconds:Integer;
 VersionByte:Byte;
 ImagesFinishedReading:Integer;
 FrameSignal,LastFrameSignal:Integer;
begin
 try
  Log(Format('DoExecute entered',[]));
  Result:=inherited DoExecute(AThread);
  if not Result then Exit;
  Result:=False;
  if AThread = nil then Exit;
  if AThread.Server = nil then Exit;

  ClockMessage.Kind:=MessageKindClock;
  ClockMessage.Clock.Seconds:=ClockGetCount div (1000*1000);
  LastFrameSignal:=ClockGetCount div (500*1000);
  ImagesFinishedReading:=FinishedWriting;
  VersionByte:=UltiboCameraTcpApiVersion;
  Check('WriteData version',AThread.Server.WriteData(@VersionByte,SizeOf(VersionByte)));

  while True do
   begin
    Seconds:=ClockGetCount div (1000*1000);
    FrameSignal:=ClockGetCount div (500*1000);
    if FrameSignal <> LastFrameSignal then
     begin
      LastFrameSignal:=FrameSignal;
      TransmitFrame(AThread);
     end
    else if Seconds <> ClockMessage.Clock.Seconds then
     begin
      ClockMessage.Clock.Seconds:=Seconds;
      Check('WriteData clock',AThread.Server.WriteData(@ClockMessage,SizeOf(ClockMessage)));
     end;
    Sleep(10);
   end;
 except on E:Exception do
  Log(Format('TListener.DoExecute Exception.Message %s',[E.Message]));
 end;
 Log(Format('DoExecute exit %s',[BoolToStr(Result)]));
end;

function CaptureLoopThread(Parameter:Pointer):PtrInt;
begin
 Result:=0;
  CameraBuffer:=Nil;
  GetMem(CameraBuffer,FrameWidth*FrameHeight*8);
  if CameraBuffer = Nil then
   begin
    Log(Format('GetMem returned 0',[]));
    ThreadHalt(0);
   end;
  StatusCode:=initcamera(FrameWidth,FrameHeight,FrameRate,Cardinal(CameraBuffer));
  Log(Format('initcamera %08.8x',[StatusCode]));
  if StatusCode >= 20 then
   begin
    StatusCode:=startcamera;
    Log(Format('startcamera %08.8x',[StatusCode]));
    if StatusCode = 0 then
     begin
      Next:=Every;
      StartedWriting:=0;
      FinishedWriting:=0;
      BeginThread(@LocalRenderLoopThread,Nil,LocalRenderLoopThreadHandle,THREAD_STACK_DEFAULT_SIZE);
      ServerStart;
      while True do
       begin
        if Filled then
         begin
          if Frames >= Next then
           begin
            Inc(StartedWriting);
            Move(CameraBuffer^,ImageQueue[StartedWriting mod NumberOfImages],ImageSize);
            Filled:=False;
            Inc(FinishedWriting);
            Inc(Next,Every);
           end
          else
           Filled:=False;
         end;
        ThreadSleep(1);
       end;
     end;
   end;
end;

begin
 Console1:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_TOPLEFT,True);
 Console2:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_BOTTOMLEFT,False);
 CameraConsole:=GraphicsWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_TOPRIGHT);
 RestoreBootFile('default','config.txt');
 BeginThread(@KeyboardLoopThread,Nil,KeyboardLoopThreadHandle,THREAD_STACK_DEFAULT_SIZE);
 BeginThread(@CaptureLoopThread,Nil,CaptureLoopThreadHandle,THREAD_STACK_DEFAULT_SIZE);
 ConsoleWindowWriteLn(Console2,'Waiting for IP addres ...');
 ConsoleWindowWriteLn(Console2,Format('IP Address is %s',[WaitForIpComplete]));
 ThreadHalt(0);
end.
