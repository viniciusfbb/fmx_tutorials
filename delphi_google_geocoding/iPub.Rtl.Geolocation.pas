unit iPub.Rtl.Geolocation;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.Types,
  System.Math;

type
  { TipSimpleGeolocation }

  TipSimpleGeolocation = packed record
  private
    FLatitude: Double;
    FLongitude: Double;
    procedure SetLatitude(AValue: Double);
    procedure SetLongitude(AValue: Double);
  public
    class function Create: TipSimpleGeolocation; overload; static;
    class function Create(const ALatitude, ALongitude: Double): TipSimpleGeolocation; overload; static;
    function DistanceInKmTo(const AGeolocation: TipSimpleGeolocation): Double;
    function DistanceInMetersTo(const AGeolocation: TipSimpleGeolocation): Double;
    function IsEmpty: Boolean;
    function IsSame(const AGeolocation: TipSimpleGeolocation): Boolean; overload;
    function IsSame(const ALatitude, ALongitude: Double): Boolean; overload;
    property Latitude: Double read FLatitude write SetLatitude;
    property Longitude: Double read FLongitude write SetLongitude;
  end;

implementation

uses
  { Delphi }
  FMX.Types;

type
  { TipEpsilon }

  TipEpsilon = record
  const
    Geolocation = 1E-6;
  end;

  { TipRoundTo }

  TipRoundTo = record
  const
    Geolocation = -6;
  end;

{ TipSimpleGeolocation }

class function TipSimpleGeolocation.Create(const ALatitude, ALongitude: Double): TipSimpleGeolocation;
begin
  Result.Latitude := ALatitude;
  Result.Longitude := ALongitude;
end;

class function TipSimpleGeolocation.Create: TipSimpleGeolocation;
begin
  Result.Latitude := 0.0;
  Result.Longitude := 0.0;
end;

function TipSimpleGeolocation.DistanceInKmTo(
  const AGeolocation: TipSimpleGeolocation): Double;
begin
  Result := DistanceInMetersTo(AGeolocation) / 1000;
end;

function TipSimpleGeolocation.DistanceInMetersTo(
  const AGeolocation: TipSimpleGeolocation): Double;
const
  EARTHS_RADIUS_IN_METERS = 6378137;
var
  LDeltaLat, LDeltaLong, LA: Double;
begin
  LDeltaLat := DegToRad(AGeolocation.Latitude - Latitude);
  LDeltaLong := DegToRad(AGeolocation.Longitude - Longitude);
  LA := Sin(LDeltaLat / 2) * Sin(LDeltaLat / 2) + Cos(DegToRad(Latitude)) * Cos(DegToRad(AGeolocation.Latitude)) * Sin(LDeltaLong / 2) * Sin(LDeltaLong / 2);
  Result := Abs(EARTHS_RADIUS_IN_METERS * 2 * ArcTan2(Sqrt(LA), Sqrt(1 - LA)));
end;

function TipSimpleGeolocation.IsEmpty: Boolean;
begin
  Result := SameValue(Latitude, 0.0, TipEpsilon.Geolocation) and
    SameValue(Longitude, 0.0, TipEpsilon.Geolocation);
end;

function TipSimpleGeolocation.IsSame(
  const AGeolocation: TipSimpleGeolocation): Boolean;
begin
  Result := IsSame(AGeolocation.Latitude, AGeolocation.Longitude);
end;

function TipSimpleGeolocation.IsSame(const ALatitude, ALongitude: Double): Boolean;
begin
  Result := SameValue(Latitude, ALatitude, TipEpsilon.Geolocation) and
    SameValue(Longitude, ALongitude, TipEpsilon.Geolocation);
end;

procedure TipSimpleGeolocation.SetLatitude(AValue: Double);
begin
  FLatitude := RoundTo(EnsureRange(AValue, -90, 90), TipRoundTo.Geolocation);
end;

procedure TipSimpleGeolocation.SetLongitude(AValue: Double);
begin
  FLongitude := RoundTo(EnsureRange(AValue, -180, 180), TipRoundTo.Geolocation);
end;

end.
