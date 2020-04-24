program PGeolocation;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  iPub.Rtl.Geolocation.Google in '..\iPub.Rtl.Geolocation.Google.pas',
  iPub.Rtl.Geolocation in '..\iPub.Rtl.Geolocation.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
