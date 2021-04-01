# Google Geocoding
This is a tutorial of how to use the google maps geocoding API to get the address of an geographic coordinates, and the reverse, to get the geographic coordinates of an address with delphi. This code is full cross platform.

##### Get the google maps geocoding api key
First of all, you need to get your own google maps geocoding api key. This is an easy step, and you will find this searching in the google.

### Geographic coordinates to address

  ```delphi
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
  ```
Replace the ```okokokokokokok``` with your api key.
    
### Address to geographic coordinates

  ```delphi
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
  ```

Replace the ```okokokokokokok``` with your api key.