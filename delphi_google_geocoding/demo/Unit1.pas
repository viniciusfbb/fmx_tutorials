unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls;

type
  TForm1 = class(TForm)
    btnGeolocationToAddress: TButton;
    btnAddressToGeolocation: TButton;
    procedure btnGeolocationToAddressClick(Sender: TObject);
    procedure btnAddressToGeolocationClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  iPub.Rtl.Geolocation,
  iPub.Rtl.Geolocation.Google;

procedure TForm1.btnGeolocationToAddressClick(Sender: TObject);
var
  LGeolocation: TipSimpleGeolocation;
  LGoogleGeocoding: IipGoogleGeocoding;
  LStreetNumber: string;
  LRoute: string;
  LSublocality: string;
  LLocality: string;
  LAdministrativeArea1: string;
  LCountry: string;
  LPostalCode: string;
  LFormattedAddress: string;
begin
  LGeolocation.Latitude := 40.7581389;
  LGeolocation.Longitude := -73.9773581;

  LGoogleGeocoding := TipGoogleGeocoding.Create;
  LGoogleGeocoding.ApiKey := 'okokokokokokok'; // Your google maps API key here <<<<<<<<<<<<<<<<<<<<
  if LGoogleGeocoding.TryGeolocationToAddress(LGeolocation, 'pt-BR', LStreetNumber,
    LRoute, LSublocality, LLocality, LAdministrativeArea1, LCountry, LPostalCode,
    LFormattedAddress) then
  begin
    showmessage('Successful!' + #13#10 + #13#10 + LFormattedAddress)
  end
  else
    showmessage('Failed!');
end;

procedure TForm1.btnAddressToGeolocationClick(Sender: TObject);
var
  LGeolocation: TipSimpleGeolocation;
  LGoogleGeocoding: IipGoogleGeocoding;
begin
  LGoogleGeocoding := TipGoogleGeocoding.Create;
  LGoogleGeocoding.ApiKey := 'okokokokokokok'; // Your google maps API key here <<<<<<<<<<<<<<<<<<<<
  if LGoogleGeocoding.TryAddressToGeolocation('625 5th Ave, New York, NY 10022, EUA', 'pt-BR', LGeolocation) then
  begin
    showmessage(Format('Successful!' + #13#10 + #13#10 + 'Latitude: %g' + #13#10 + 'Longitude: %g',
      [LGeolocation.Latitude, LGeolocation.Longitude]));
  end
  else
    showmessage('Failed!');
end;

end.
