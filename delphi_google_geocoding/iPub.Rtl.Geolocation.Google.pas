unit iPub.Rtl.Geolocation.Google;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.Net.HttpClient,

  { iPub }
  iPub.Rtl.Geolocation;

type
  { IipGoogleGeocoding }

  IipGoogleGeocoding = interface(IUnknown)
    ['{35162A15-67AD-4381-8161-92D59DA962A7}']
    function GetApiKey: string;
    procedure SetApiKey(const AValue: string);
    function TryAddressToGeolocation(const AAddress, ALanguage: string; out AGeolocation: TipSimpleGeolocation): Boolean;
    function TryGeolocationToAddress(const AGeolocation: TipSimpleGeolocation; const ALanguage: string; var AStreetNumber, ARoute, ASublocality, ALocality, AAdministrativeArea1, ACountry, APostalCode, AFormattedAddress: string): Boolean;
    property ApiKey: string read GetApiKey write SetApiKey;
  end;

  { TipGoogleGeocoding }

  TipGoogleGeocoding = class(TInterfacedObject, IipGoogleGeocoding)
  private
    FApiKey: string;
    FHttpClient: THTTPClient;
    function GetApiKey: string;
    procedure SetApiKey(const AValue: string);
  public
    constructor Create(const AApiKey: string = '');
    function TryAddressToGeolocation(const AAddress, ALanguage: string; out AGeolocation: TipSimpleGeolocation): Boolean;
    function TryGeolocationToAddress(const AGeolocation: TipSimpleGeolocation; const ALanguage: string; var AStreetNumber, ARoute, ASublocality, ALocality, AAdministrativeArea1, ACountry, APostalCode, AFormattedAddress: string): Boolean;
    property ApiKey: string read GetApiKey write SetApiKey;
  end;

implementation

uses
  { Delphi }
  System.Generics.Collections,
  System.JSON,
  System.Net.URLClient,
  System.NetEncoding,
  System.NetConsts,
  System.SysUtils;

{ TipGoogleGeocoding }

constructor TipGoogleGeocoding.Create(const AApiKey: string);
begin
  inherited Create;
  FApiKey := AApiKey;
  FHttpClient := THTTPClient.Create;
  FHttpClient.Accept := 'application/json';
  FHttpClient.AcceptCharSet := 'UTF-8';
  FHttpClient.AllowCookies := False;
  FHttpClient.ConnectionTimeout := 10000;
  FHttpClient.HandleRedirects := False;
  FHttpClient.ResponseTimeout := 10000;
  FHttpClient.SecureProtocols := [THttpSecureProtocol.TLS12];
end;

function TipGoogleGeocoding.GetApiKey: string;
begin
  Result := FApiKey;
end;

procedure TipGoogleGeocoding.SetApiKey(const AValue: string);
begin
  FApiKey := AValue;
end;

function TipGoogleGeocoding.TryAddressToGeolocation(const AAddress, ALanguage: string;
  out AGeolocation: TipSimpleGeolocation): Boolean;
const
  GOOGLE_ADDRESS_TO_GEOLOCATION_URI = 'https://maps.googleapis.com/maps/api/geocode/json?address=%s&key=%s';
var
  LLatitude: Double;
  LLongitude: Double;
  LResponse: IHTTPResponse;
  LResultsArray: TJSONArray;
  LValue: TJSONValue;
begin
  AGeolocation := TipSimpleGeolocation.Create;
  if FApiKey.IsEmpty then
    Exit(False);
  FHttpClient.AcceptLanguage := 'en-US;q=0.5';
  if not ALanguage.Trim.IsEmpty then
    FHttpClient.AcceptLanguage := ALanguage + ',' + FHttpClient.AcceptLanguage;
  try
    LResponse := FHttpClient.Get(Format(GOOGLE_ADDRESS_TO_GEOLOCATION_URI, [TNetEncoding.URL.EncodeQuery(AAddress), FApiKey]));
  except
    on E: ENetException do
      Exit(False);
  end;
  LValue := TJSONObject.ParseJSONValue(LResponse.ContentAsString);
  try
    if (not Assigned(LValue)) or (LValue.GetValue<string>('status', '') <> 'OK') then
      Exit(False);
    LResultsArray := LValue.P['results'] as TJSONArray;
    if LResultsArray.Count <= 0 then
      Exit(False);
    Result := (Assigned(LResultsArray.Items[0].FindValue('geometry'))) and (Assigned(LResultsArray.Items[0].P['geometry'].FindValue('location')));
    if Result then
    begin
      Result := (LResultsArray.Items[0].P['geometry'].P['location'].TryGetValue<Double>('lat', LLatitude)) and (LResultsArray.Items[0].P['geometry'].P['location'].TryGetValue<Double>('lng', LLongitude));
      if Result then
      begin
        AGeolocation.Latitude := LLatitude;
        AGeolocation.Longitude := LLongitude;
      end;
    end;
  finally
    LValue.Free;
  end;
end;

function TipGoogleGeocoding.TryGeolocationToAddress(
  const AGeolocation: TipSimpleGeolocation; const ALanguage: string; var AStreetNumber,
  ARoute, ASublocality, ALocality, AAdministrativeArea1, ACountry,
  APostalCode, AFormattedAddress: string): Boolean;

  function ProcessAddressComponent(AItem: TJSONObject): Boolean;
  var
    LValue: TJSONValue;
    LTypesArray: TJSONArray;
    I: Integer;
  begin
    LValue := AItem.GetValue('types');
    if (not Assigned(LValue)) or not (LValue is TJSONArray) then
      Exit(False);
    Result := True;
    LTypesArray := TJSONArray(LValue);
    for I := 0 to LTypesArray.Count-1 do
    begin
      if LTypesArray.Items[I].Value = 'street_number' then
      begin
        if not AItem.TryGetValue<string>('long_name', AStreetNumber) then
          Exit(False);
      end
      else if LTypesArray.Items[I].Value = 'route' then
      begin
        if not AItem.TryGetValue<string>('long_name', ARoute) then
          Exit(False);
      end
      else if LTypesArray.Items[I].Value = 'sublocality' then
      begin
        if not AItem.TryGetValue<string>('long_name', ASublocality) then
          Exit(False);
      end
      else if (LTypesArray.Items[I].Value = 'locality') and (ALocality = '') then
      begin
        if not AItem.TryGetValue<string>('long_name', ALocality) then
          Exit(False);
      end
      else if (LTypesArray.Items[I].Value = 'administrative_area_level_2') and (ALocality = '') then
      begin
        if not AItem.TryGetValue<string>('long_name', ALocality) then
          Exit(False);
      end
      else if LTypesArray.Items[I].Value = 'administrative_area_level_1' then
      begin
        if not AItem.TryGetValue<string>('short_name', AAdministrativeArea1) then
          Exit(False);
      end
      else if LTypesArray.Items[I].Value = 'country' then
      begin
        if not AItem.TryGetValue<string>('long_name', ACountry) then
          Exit(False);
      end
      else if LTypesArray.Items[I].Value = 'postal_code' then
      begin
        if not AItem.TryGetValue<string>('long_name', APostalCode) then
          Exit(False);
      end;
    end;
  end;

const
  GOOGLE_GEOLOCATION_TO_ADDRESS_URI = 'https://maps.googleapis.com/maps/api/geocode/json?latlng=%s,%s&key=%s';
var
  I: Integer;
  LResponse: IHTTPResponse;
  LStatus: string;
  LJSON: TJSONValue;
  LJSONResultsValue: TJSONValue;
begin
  Result := False;
  FHttpClient.AcceptLanguage := 'en-US;q=0.5';
  if not ALanguage.Trim.IsEmpty then
    FHttpClient.AcceptLanguage := ALanguage + ',' + FHttpClient.AcceptLanguage;
  try
    if FApiKey.IsEmpty then
      Exit(False);
    try
      LResponse := FHttpClient.Get(Format(GOOGLE_GEOLOCATION_TO_ADDRESS_URI, [AGeolocation.Latitude.ToString.Replace(',', '.'), AGeolocation.Longitude.ToString.Replace(',', '.'), FApiKey]));
    except
      on E: ENetException do
        Exit(False);
    end;
    LJSON := TJSONObject.ParseJSONValue(LResponse.ContentAsString);
    if not Assigned(LJSON) then
      Exit(False);
    try
      if (not LJSON.TryGetValue<string>('status', LStatus)) or (LStatus <> 'OK') or not (LJSON is TJSONObject) then
        Exit(False);
      LJSONResultsValue := TJSONObject(LJSON).GetValue('results');
      if (not Assigned(LJSONResultsValue)) or (not (LJSONResultsValue is TJSONArray)) or (TJSONArray(LJSONResultsValue).Count <= 0) then
        Exit(False);
      LJSONResultsValue := TJSONObject(TJSONArray(LJSONResultsValue).Items[0]).GetValue('address_components');
      if (not Assigned(LJSONResultsValue)) or (not (LJSONResultsValue is TJSONArray)) or (TJSONArray(LJSONResultsValue).Count <= 0) then
        Exit(False);
      for I := 0 to TJSONArray(LJSONResultsValue).Count-1 do
        if (not (TJSONArray(LJSONResultsValue).Items[I] is TJSONObject)) or
           (not ProcessAddressComponent(TJSONObject(TJSONArray(LJSONResultsValue).Items[I]))) then
          Exit(False);
      Result := LJSON.TryGetValue<string>('results[0].formatted_address', AFormattedAddress) and
       (AFormattedAddress <> '') and (ACountry <> '') and (AAdministrativeArea1 <> '') and
       (ALocality <> '') and (ASublocality <> '');
    finally
      LJSON.Free;
    end;
  finally
    if not Result then
    begin
      AStreetNumber := '';
      ARoute := '';
      ASublocality := '';
      ALocality := '';
      AAdministrativeArea1 := '';
      ACountry := '';
      APostalCode := '';
      AFormattedAddress := '';
    end;
  end;
end;

end.
