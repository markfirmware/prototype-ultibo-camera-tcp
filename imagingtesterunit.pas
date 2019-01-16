unit ImagingTesterUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, GR32_Image, blcksock, ImagingDeviceUnit, StrUtils, Math;

type

  { TForm1 }

  TForm1 = class(TForm)
    StreamButton: TButton;
    EditIpAddress: TEdit;
    Image32: TImage32;
    StreamStopButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure StreamButtonClick(Sender: TObject);
    procedure StreamStopButtonClick(Sender: TObject);
  private
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

var
  SelectImage:Boolean=False;
  Client: TTCPBlockSocket;

var
  StatusCode: Integer = 0;

const
  BufferSize=16*1024*1024;
var
  Streaming:Boolean;
  BytesReceived:Integer;
  ImageBuffer:Array [0 .. BufferSize - 1] of Byte;

procedure TForm1.StreamButtonClick(Sender: TObject);
var
  Delta:Integer;
  ApiVersion:Byte;
  ImagingMessage:TImagingMessage;
  ImagesReceived:Integer;
  Finished:Integer;
  ClocksReceived,Seconds:Integer;
  GrayScale:Byte;
  X,Y:Integer;
begin
  Client.Connect (EditIpAddress.Text, '27000');
//  Client.Connect ('10.16.33.106', '27000'); // lab indy iot200
//  Client.Connect ('10.16.33.110', '27000'); // raindrop one right
//  Client.Connect ('10.16.33.125', '27000'); // raindrop one left
  BytesReceived:=Client.RecvBuffer (@ApiVersion, 1);
  if (BytesReceived <> 1) or (ApiVersion <> UltiboCameraTcpApiVersion) then
   raise Exception.Create('wrong api version');
  Streaming:=True;
  ImagesReceived:=0;
  BytesReceived:=0;
  EditIpAddress.Enabled:=False;
  StreamStopButton.Enabled:=True;
  ClocksReceived:=0;
  while Streaming do
    begin
      Application.ProcessMessages;
      Delta:=Client.RecvBuffer (@ImagingMessage, SizeOf(ImagingMessage));
      Inc(BytesReceived,Delta);
      case ImagingMessage.Kind of
      MessageKindClock:
        begin
          Inc(ClocksReceived);
          Seconds:=ImagingMessage.Clock.Seconds;
        end;
      MessageKindImage:
        begin
          Finished:=0;
          while Finished < ImagingMessage.Image.DataLength do
            begin
              Delta:=Client.RecvBuffer (@ImageBuffer[Finished], ImagingMessage.Image.DataLength - Finished);
              Inc(Finished,Delta);
              Inc(BytesReceived,Delta);
            end;
          for X:=0 to ImagingMessage.Image.LengthX - 1 do
            for Y:=0 to ImagingMessage.Image.LengthY - 1 do
              begin
                GrayScale:=ImageBuffer[Y*ImagingMessage.Image.LengthX + X];
                Image32.Bitmap.Pixel[X,Y]:=RGBToColor(GrayScale,GrayScale,GrayScale);
              end;
          Image32.Refresh;
          Inc(ImagesReceived);
        end;
      end;
    end;
  Client.CloseSocket;
  EditIpAddress.Enabled:=True;
end;


procedure TForm1.StreamStopButtonClick(Sender: TObject);
begin
  Streaming:=False;
  StreamStopButton.Enabled:=False;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Image32.SetupBitmap();
  Streaming:=False;
  Client := TTCPBlockSocket.Create;
end;

end.

