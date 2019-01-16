unit ImagingDeviceUnit;
{$mode objfpc}{$H+}

interface

const
 UltiboCameraTcpApiVersion = 0;
 MessageKindClock = 0;
 MessageKindImage = 1;

type
  TClock = packed record
   Seconds:Integer;
  end;
  TImage = packed record
   DataLength:Integer;
   FrameNumber:Integer;
   LengthX:Integer;
   LengthY:Integer;
  end;

  TImagingMessage = packed record
   case Kind:Integer of
    MessageKindClock:(Clock:TClock);
    MessageKindImage:(Image:TImage);
  end;

implementation

end.

