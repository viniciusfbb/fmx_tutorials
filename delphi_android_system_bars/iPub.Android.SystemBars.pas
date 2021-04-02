// [iPub - github.com/viniciusfbb] - 01/04/2021 - Delphi 10.4.1
// https://github.com/viniciusfbb/fmx_tutorials/tree/master/delphi_android_system_bars/
unit iPub.Android.SystemBars;

interface

{$SCOPEDENUMS ON}
{$IFDEF ANDROID}

uses
  { Delphi }
  System.UITypes;

type
  { TAndroidSystemBars }

  TAndroidSystemBars = record
  public
    class function GetNavigationBarHeight: Single; static;
    class function GetStatusBarHeight: Single; static;
    // We will try to remove the system bars (status bar and navigation bar)
    // background (ignoring ANearStatusBarColor, ANearNavigationBarColor
    // parameters) and set the app layout to cover the entire screen, that is,
    // the form will expand to below the status bar and navigation bar. However
    // in older android versions we don't have support to transparent status bar
    // or transparent navigation bar, but some versions have the option to set
    // an opaque color to system bars. When possible we will adjust
    // automatically the light of system bars content (system bars foreground
    // color) checking the brightness of ANearStatusBarColor and
    // ANearNavigationBarColor to set the dark or light foreground. Notes:
    // 1) don't use this before the OnCreate of main form
    // 2) you need to call GetStatusBarHeight and GetNavigationBarHeight to get
    // the system bars height to set the padding in your form to avoid some
    // controls like TEdit or TButton to go below the system bar
    class procedure RemoveSystemBarsBackground(ANearStatusBarColor, ANearNavigationBarColor: TAlphaColor); static;
  end;

  {$REGION 'SPLASH SCREEN - system bars and background'}
(*
The class TAndroidSystemBars is just to use in runtime, after the application full
launch. But if you don't change the default "styles.xml" and "splash_image_def.xml"
provided by Delphi, you will have an ugly system bars or simply not predictable
(some devices may be white, others black or even gray), and the splash screen
background will be always black. To fix the system bars colors and background
color of splash screen you should change the "styles.xml" and "splash_image_def.xml" files.

#### Creating "styles.xml"

Create one file called "styles.xml" with the following content:

<resources xmlns:android="http://schemas.android.com/apk/res/android">
    <style name="AppTheme" parent="@android:style/Theme.Material.Light.NoActionBar">
        <item name="android:navigationBarColor">#01ffffff</item>
        <item name="android:statusBarColor">#00000000</item>
        <item name="android:colorPrimary">#ffffff</item>
        <item name="android:windowBackground">@drawable/splash_image_def</item>
    </style>
</resources>

In this content above I used the white color example (#ffffff) but you can change it to
your color but note that in navigationBarColor the color need to start with "#01" to set
the alpha value to 1 to solve problems found in LG devices.
Save the file in one subdirectory of your project, like "resources" subdirectory, for example.

#### Creating "splash_image_def.xml"

Create one file called "splash_image_def.xml" with the following content:

<?xml version="1.0" encoding="utf-8"?>
<layer-list xmlns:android="http://schemas.android.com/apk/res/android">
  <item>
    <shape android:shape="rectangle">
      <solid android:color="#ffffff" />
    </shape>
  </item>
  <item>
    <bitmap android:gravity="center" android:src="@drawable/splash_image" />
  </item>
</layer-list>

In this content above I used one white color example (#ffffff) but you can change it to your color.
Save the file in one subdirectory of your project, like "resources" subdirectory, for example.

#### Deploying the "styles.xml" and "splash_image_def.xml"

 1) Open your Project > Deployment
 2) Add your own "styles.xml" file and set the Remote Path field to "res\values"
 3) Add your own "splash_image_def.xml" file and set the Remote Path field to "res\drawable"

Now you can uninstall your app, compile and run to see the difference ;)
*)
  {$ENDREGION}

implementation

uses
  { Delphi }
  System.SysUtils,
  FMX.Platform,
  FMX.Platform.Android,
  Androidapi.Helpers,
  Androidapi.Jni,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Os,
  Androidapi.JNI.App,
  Androidapi.JNIBridge,
  Androidapi.JNI.GraphicsContentViewText;

function GetScreenScale: Single;
var
  LScreenService: IFMXScreenService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, LScreenService) then
    Result := LScreenService.GetScreenScale
  else
    Result := 1;
end;

class function TAndroidSystemBars.GetNavigationBarHeight: Single;

  function GetAndroidAbsoluteNavigationBarHeight: Single;

    function IsTablet: Boolean;
    begin
      Result := (TAndroidHelper.Context.getResources().getConfiguration().screenLayout and TJConfiguration.JavaClass.SCREENLAYOUT_SIZE_MASK) >= TJConfiguration.JavaClass.SCREENLAYOUT_SIZE_LARGE;
    end;

  var
    LResources: JResources;
    LNavigationBarExists: Boolean;
    LOrientation: Integer;
    LResourceID: Integer;
  begin
    Result := 0;
    LResources := TAndroidHelper.Context.getResources();
    try
      LResourceID := LResources.getIdentifier(TAndroidHelper.StringToJString('config_showNavigationBar'), TAndroidHelper.StringToJString('bool'), TAndroidHelper.StringToJString('android'));
      LNavigationBarExists := (LResourceID > 0) and LResources.getBoolean(LResourceID);
    except
      LNavigationBarExists := False;
    end;
    if LNavigationBarExists then
    begin
      LOrientation := LResources.getConfiguration().orientation;
      if IsTablet then
      begin
        if LOrientation = TJConfiguration.JavaClass.ORIENTATION_PORTRAIT then
          LResourceID := LResources.getIdentifier(TAndroidHelper.StringToJString('navigation_bar_height'), TAndroidHelper.StringToJString('dimen'), TAndroidHelper.StringToJString('android'))
        else
          LResourceID := LResources.getIdentifier(TAndroidHelper.StringToJString('navigation_bar_height_landscape'), TAndroidHelper.StringToJString('dimen'), TAndroidHelper.StringToJString('android'));
      end
      else
      begin
        if LOrientation = TJConfiguration.JavaClass.ORIENTATION_PORTRAIT then
          LResourceID := LResources.getIdentifier(TAndroidHelper.StringToJString('navigation_bar_height'), TAndroidHelper.StringToJString('dimen'), TAndroidHelper.StringToJString('android'))
        else
          LResourceID := LResources.getIdentifier(TAndroidHelper.StringToJString('navigation_bar_width'), TAndroidHelper.StringToJString('dimen'), TAndroidHelper.StringToJString('android'));
      end;
      if LResourceID > 0 then
        Result := LResources.getDimensionPixelSize(LResourceID);
    end;
  end;

begin
  Result := Round(GetAndroidAbsoluteNavigationBarHeight / GetScreenScale);
end;

class function TAndroidSystemBars.GetStatusBarHeight: Single;

  function GetAndroidAbsoluteStatusBarHeight: Single;
  var
    resourceID: Integer;
    sAbis: string;
    arrAbis: TJavaObjectArray<JString>;
    I: Integer;
    needCheckStatusBarHeight: boolean;
  begin
    Result := 0;
    if TOSVersion.Major >= 5 then
    begin
      sAbis := '';
      arrAbis := TJBuild.JavaClass.SUPPORTED_ABIS;
      for I := 0 to arrAbis.Length - 1 do
        sAbis := sAbis + ',' + JStringToString(arrAbis.Items[I]);
      sAbis := sAbis.trim([',']);
    end
    else
      sAbis := JStringToString(TJBuild.JavaClass.CPU_ABI) + ',' + JStringToString(TJBuild.JavaClass.CPU_ABI2);

    needCheckStatusBarHeight := (sAbis.Contains('x86') or JStringToString(TJBuild.JavaClass.FINGERPRINT).Contains('intel')
      or sAbis.Contains('arm64-v8a')) and (TOSVersion.Major >= 4);

    if (TOSVersion.Major >= 5) or (needCheckStatusBarHeight) then
    begin
      resourceID := TAndroidHelper.Activity.getResources.getIdentifier(StringToJString('status_bar_height'),
        StringToJString('dimen'), StringToJString('android'));
      if resourceID > 0 then
        Result := TAndroidHelper.Activity.getResources.getDimensionPixelSize(resourceID);
    end;
  end;

begin
  Result := Round(GetAndroidAbsoluteStatusBarHeight / GetScreenScale);
end;

{$REGION ' Considerations'}
// We will try to remove the system bars (status bar and navigation bar)
// background (ignoring ANearStatusBarColor, ANearNavigationBarColor
// parameters) and set the app layout to cover the entire screen, that is,
// the form will expand to below the status bar and navigation bar. However
// in older android versions we don't have support to transparent status bar
// or transparent navigation bar, but some versions have the option to set
// an opaque color to system bars. When possible we will adjust
// automatically the light of system bars content (system bars foreground
// color) checking the brightness of ANearStatusBarColor and
// ANearNavigationBarColor to set the dark or light foreground. Notes:
// 1) don't use this before the OnCreate of main form
// 2) you need to call GetStatusBarHeight and GetNavigationBarHeight to get the
// system bars height to set the padding in your form to avoid some controls
// like TEdit or TButton to go below the system bar
// Tested ides: Delphi Sydney 10.4.1
// Tested devices:
// LG      - LM-X430 Android32 10.0.0 (API level 29)
// SAMSUNG - SM-G955F Android64 9.0.0 (API level 28)
// SAMSUNG - SM-G935F Android64 8.0.0 (API level 26)
// All worked perfectly. But these codes should work well on all android
// versions supported by delphi (Android 6 to Android 11)
{$ENDREGION}
class procedure TAndroidSystemBars.RemoveSystemBarsBackground(ANearStatusBarColor, ANearNavigationBarColor: TAlphaColor);

  function AlphaColorToJColor(AColor: TAlphaColor): Integer;
  var
    LColorRec: TAlphaColorRec;
  begin
    LColorRec := TAlphaColorRec.Create(AColor);
    Result := TJColor.JavaClass.argb(LColorRec.A, LColorRec.R, LColorRec.G, LColorRec.B);
  end;

var
  LStatusBarLight: Boolean;
  LNavigationBarLight: Boolean;
  LSystemUiVisibility: Integer;
  LStatusBarJColor: Integer;
  LNavigationBarJColor: Integer;
  LWinParams: JWindowManager_LayoutParams;
  LWindow: JWindow;
  LView: JView;
begin
  // We will disconsider the alpha
  TAlphaColorRec(ANearStatusBarColor).A := 255;
  TAlphaColorRec(ANearNavigationBarColor).A := 255;

  // Calculating the brightness of new system bars background
  LStatusBarLight := (TAlphaColorRec(ANearStatusBarColor).R + TAlphaColorRec(ANearStatusBarColor).G +
    TAlphaColorRec(ANearStatusBarColor).B) > 382;
  LNavigationBarLight := (TAlphaColorRec(ANearNavigationBarColor).R + TAlphaColorRec(ANearNavigationBarColor).G +
    TAlphaColorRec(ANearNavigationBarColor).B) > 382;

  // Initializing the local variables
  LSystemUiVisibility := 0;
  LStatusBarJColor := AlphaColorToJColor(ANearStatusBarColor);
  LNavigationBarJColor := AlphaColorToJColor(ANearNavigationBarColor);
  if Assigned(TAndroidHelper.Activity) then
    LWindow := TAndroidHelper.Activity.getWindow
  else
    LWindow := nil;
  if MainActivity <> nil then
    LView := MainActivity.getContentView
  else
    LView := nil;
  if Assigned(LWindow) then
    LWinParams := LWindow.getAttributes
  else
    LWinParams := nil;

  // Setting the configurations
  if TJBuild_VERSION.JavaClass.SDK_INT >= TJBuild_VERSION_CODES.JavaClass.M then
  begin
    if LStatusBarLight then
      LSystemUiVisibility := LSystemUiVisibility or TJView.JavaClass.SYSTEM_UI_FLAG_LIGHT_STATUS_BAR;
    LStatusBarJColor := TJColor.JavaClass.TRANSPARENT;
  end;
  if TJBuild_VERSION.JavaClass.SDK_INT >= TJBuild_VERSION_CODES.JavaClass.O then
  begin
    if LNavigationBarLight then
      LSystemUiVisibility := LSystemUiVisibility or TJView.JavaClass.SYSTEM_UI_FLAG_LIGHT_NAVIGATION_BAR;
    // This is really necessary because some devices (in my tests, in device LG LM-X430 Android32 10.0.0 (API level 29),
    // the full transparent don't work, the transparent color is replaced by a semitransparent white. However, when we avoid
    // the full transparent color, it work in all devices)
    TAlphaColorRec(ANearNavigationBarColor).A := 1;
    LNavigationBarJColor := AlphaColorToJColor(ANearNavigationBarColor);
  end;
  if TJBuild_VERSION.JavaClass.SDK_INT >= TJBuild_VERSION_CODES.JavaClass.KITKAT then
  begin
    LSystemUiVisibility := LSystemUiVisibility or
      TJView.JavaClass.SYSTEM_UI_FLAG_LAYOUT_STABLE or
      TJView.JavaClass.SYSTEM_UI_FLAG_LAYOUT_FULLSCREEN or
      TJView.JavaClass.SYSTEM_UI_FLAG_LAYOUT_HIDE_NAVIGATION;
    if Assigned(LView) then
      LView.setSystemUiVisibility(LSystemUiVisibility);
  end;
  if (TJBuild_VERSION.JavaClass.SDK_INT >= TJBuild_VERSION_CODES.JavaClass.KITKAT) and
    (TJBuild_VERSION.JavaClass.SDK_INT < TJBuild_VERSION_CODES.JavaClass.LOLLIPOP) and
    Assigned(LWinParams) then
  begin
    LWinParams.flags := LWinParams.flags or
      TJWindowManager_LayoutParams.JavaClass.FLAG_TRANSLUCENT_STATUS or
      TJWindowManager_LayoutParams.JavaClass.FLAG_TRANSLUCENT_NAVIGATION;
  end;
  if TJBuild_VERSION.JavaClass.SDK_INT >= TJBuild_VERSION_CODES.JavaClass.LOLLIPOP then
  begin
    if Assigned(LWinParams) then
    begin
      LWinParams.flags := LWinParams.flags and not
        (TJWindowManager_LayoutParams.JavaClass.FLAG_TRANSLUCENT_STATUS or
         TJWindowManager_LayoutParams.JavaClass.FLAG_TRANSLUCENT_NAVIGATION);
    end;
    if Assigned(LWindow) then
    begin
      LWindow.setStatusBarColor(LStatusBarJColor);
      LWindow.setNavigationBarColor(LNavigationBarJColor);
    end;
  end;
  if Assigned(LWindow) and Assigned(LWinParams) then
    LWindow.setAttributes(LWinParams);
  if Assigned(LView) then
    LView.setFitsSystemWindows(False);
end;

{$ELSE}
implementation
{$ENDIF}
end.
