// [iPub - github.com/viniciusfbb] - 01/04/2021 - Delphi 10.4.1
// https://github.com/viniciusfbb/fmx_tutorials/tree/master/delphi_android_system_bars/
unit iPub.Android.SystemBars;

interface

{$SCOPEDENUMS ON}
{$IFDEF ANDROID}

uses
  { Delphi }
  System.Classes,
  System.Messaging,
  System.Types,
  System.UITypes,
  Androidapi.JNIBridge,
  Androidapi.JNI.GraphicsContentViewText;

type
  TAndroidInsetsChangeMessage = class(System.Messaging.TMessage);

  { TAndroidSystemBars }

  TAndroidSystemBars = record
  {$REGION 'private'}
  strict private
    type
      TOnApplyWindowInsetsListener = class(TJavaLocal, JView_OnApplyWindowInsetsListener)
      public
        function onApplyWindowInsets(v: JView; insets: JWindowInsets): JWindowInsets; cdecl;
      end;
    class var
      FInitialized: Boolean;
      FInsetsChangeMessageId: Integer;
      FListener: TOnApplyWindowInsetsListener;
      FMainFormChangedMessageId: Integer;
      FOnInsetsChange: TNotifyEvent;
    class function AbsoluteSystemInsets: TRect; static;
    class function AbsoluteTappableInsets: TRect; static;
    class function AbsoluteToScaled(const AAbsoluteRect: TRect): TRectF; static;
    class procedure InsetsChangeHandler(const ASender: TObject; const AMessage: System.Messaging.TMessage); static;
    class procedure MainFormChangedHandler(const ASender: TObject; const AMessage: System.Messaging.TMessage); static;
    class constructor Create;
    class destructor Destroy;
  {$ENDREGION}
  public
    {$REGION ' - Documentation'}
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
    // 2) you need to call SystemInsets or TappableInsets to get the sizes of
    // system bars to set the padding in your form to avoid some
    // controls like TEdit or TButton to go below the system bar
    {$ENDREGION}
    class procedure RemoveSystemBarsBackground(ANearStatusBarColor, ANearNavigationBarColor: TAlphaColor); static;
    // Area of a full-screen window that is partially or fully obscured by the status bar, navigation bar, IME or other system windows.
    // Basically is the sizes of StatusBar, NavigationBar and GestureBar
    class function SystemInsets: TRectF; static;
    // Represent how much tappable elements must at least be inset to remain both tappable and visually unobstructed by persistent system windows.
    // Basically is the sizes of StatusBar and NavigationBar (the gesture bar size is ignored)
    class function TappableInsets: TRectF; static;
    class property OnInsetsChange: TNotifyEvent read FOnInsetsChange write FOnInsetsChange;
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

 1) Open your Project > Deployment, then you need to apply the following steps below for Android 32/64 bits in Release and Debug mode:
 2) Look in the "Local Name" column and disable these 3 default files: "styles.xml", "styles-v21.xml" and "splash_image_def.xml"
 3) Add your own "styles.xml" file and set the Remote Path field to "res\values"
 4) Add your own "splash_image_def.xml" file and set the Remote Path field to "res\drawable"

Now you can uninstall your app, compile and run to see the difference ;)
*)
  {$ENDREGION}

implementation

uses
  { Delphi }
  System.SysUtils,
  System.Math,
  System.Math.Vectors,
  FMX.Forms,
  FMX.Platform,
  FMX.Platform.Android,
  FMX.Platform.Screen.Android,
  Androidapi.Helpers,
  Androidapi.Jni,
  Androidapi.JNI.Embarcadero,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Os,
  Androidapi.JNI.App;

type
  { TAndroid10 }

  TAndroid10 = class
  public type
    { Insets }

    JInsetsClass = interface(JObjectClass)
      ['{BDB53B96-47AA-4A43-A08F-7648EE48A7D9}']
    end;

    [JavaSignature('android/graphics/Insets')]
    JInsets = interface(JObject)
      ['{A0703F81-D34D-4D59-8D3F-5E1D7DD3192D}']
      function _Getbottom: Integer; cdecl;
      function _Getleft: Integer; cdecl;
      function _Getright: Integer; cdecl;
      function _Gettop: Integer; cdecl;
      property bottom: Integer read _Getbottom;
      property left: Integer read _Getleft;
      property right: Integer read _Getright;
      property top: Integer read _Gettop;
    end;
    TJInsets = class(TJavaGenericImport<JInsetsClass, JInsets>) end;

    { WindowInsets }

    JWindowInsetsExClass = interface(JWindowInsetsClass)
      ['{6BCCFCCA-A7F0-4740-B4D6-D03286DAD89C}']
    end;

    [JavaSignature('android/view/WindowInsets')]
    JWindowInsetsEx = interface(JWindowInsets)
      ['{05AFC51B-3683-4DD1-BB1A-22799899142B}']
      function getTappableElementInsets: JInsets; cdecl;
    end;
    TJWindowInsetsEx = class(TJavaGenericImport<JWindowInsetsExClass, JWindowInsetsEx>) end;
  end;

  { TAndroid11 }

  TAndroid11 = class
  public type
    { WindowInsets.Type }

    JWindowInsets_TypeClass = interface(JObjectClass)
      ['{847139A7-6187-4D24-86FE-44BB6017F246}']
      {class} function captionBar: Integer; cdecl;
      {class} function displayCutout: Integer; cdecl;
      {class} function ime: Integer; cdecl;
      {class} function mandatorySystemGestures: Integer; cdecl;
      {class} function navigationBars: Integer; cdecl;
      {class} function statusBars: Integer; cdecl;
      {class} function systemBars: Integer; cdecl;
      {class} function systemGestures: Integer; cdecl;
      {class} function tappableElement: Integer; cdecl;
    end;

    [JavaSignature('android/view/WindowInsets$Type')]
    JWindowInsets_Type = interface(JObject)
      ['{380D2B14-3C81-4F30-888E-1465CF5D2BCF}']
    end;
    TJWindowInsets_Type = class(TJavaGenericImport<JWindowInsets_TypeClass, JWindowInsets_Type>) end;


    { WindowInsets }

    JWindowInsetsExClass = interface(JWindowInsetsClass)
      ['{6BCCFCCA-A7F0-4740-B4D6-D03286DAD89C}']
    end;

    [JavaSignature('android/view/WindowInsets')]
    JWindowInsetsEx = interface(JWindowInsets)
      ['{05AFC51B-3683-4DD1-BB1A-22799899142B}']
      function getInsets(typeMask: Integer): TAndroid10.JInsets; cdecl;
    end;
    TJWindowInsetsEx = class(TJavaGenericImport<JWindowInsetsExClass, JWindowInsetsEx>) end;
  end;

{$IF CompilerVersion < 34.0} // Delphi Sydney 10.4
  { TAndroidBeforeMarshmallow }

  TAndroidBeforeMarshmallow = class
  strict private
    class function GetAbsoluteNavigationBarInsets: TRect; static;
    class function GetAbsoluteStatusBarHeight: Integer; static;
  public
    class function AbsoluteSystemInsets: TRect; static;
  end;

{ TAndroidBeforeMarshmallow }

class function TAndroidBeforeMarshmallow.GetAbsoluteNavigationBarInsets: TRect;

  function IsTablet: Boolean;
  begin
    Result := (TAndroidHelper.Context.getResources().getConfiguration().screenLayout and
      TJConfiguration.JavaClass.SCREENLAYOUT_SIZE_MASK) >= TJConfiguration.JavaClass.SCREENLAYOUT_SIZE_LARGE;
  end;

type
  TNavigationBarLocation = (Right, Bottom);
var
  LResources: JResources;
  LNavigationBarExists: Boolean;
  LOrientation: Integer;
  LResourceID: Integer;
  LLocation: TNavigationBarLocation;
begin
  Result := TRect.Empty;
  try
    LLocation := TNavigationBarLocation.Bottom;
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
        begin
          LResourceID := LResources.getIdentifier(TAndroidHelper.StringToJString('navigation_bar_width'), TAndroidHelper.StringToJString('dimen'), TAndroidHelper.StringToJString('android'));
          LLocation := TNavigationBarLocation.Right;
        end;
      end;
      if LResourceID > 0 then
      begin
        case LLocation of
          TNavigationBarLocation.Right: Result.Right := LResources.getDimensionPixelSize(LResourceID);
          TNavigationBarLocation.Bottom: Result.Bottom := LResources.getDimensionPixelSize(LResourceID);
        end;
      end;
    end;
  except
    Result := TRect.Empty;
  end;
end;

class function TAndroidBeforeMarshmallow.GetAbsoluteStatusBarHeight: Integer;
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

class function TAndroidBeforeMarshmallow.AbsoluteSystemInsets: TRect;
begin
  Result := GetAbsoluteNavigationBarInsets;
  Result.Top := GetAbsoluteStatusBarHeight;
end;
{$ENDIF}

{ TAndroidSystemBars.TOnApplyWindowInsetsListener }

function TAndroidSystemBars.TOnApplyWindowInsetsListener.onApplyWindowInsets(v: JView;
  insets: JWindowInsets): JWindowInsets;
begin
  Result := insets;
  TThread.ForceQueue(nil,
    procedure
    begin
      TMessageManager.DefaultManager.SendMessage(nil, TAndroidInsetsChangeMessage.Create);
    end);
end;

{ TAndroidSystemBars }

class function TAndroidSystemBars.AbsoluteSystemInsets: TRect;
var
  LMainActivity: JFMXNativeActivity;
  LViewGroup: JViewGroup;
  LWindowInsets: JWindowInsets;
  LInsets: TAndroid10.JInsets;
begin
  Result := TRect.Empty;
  if TOSVersion.Check(6) then // Android 6 (Marshmallow / Api level 23) or later
  begin
    LMainActivity := MainActivity;
    if LMainActivity <> nil then
    begin
      {$IF CompilerVersion >= 34.0} // Delphi Sydney 10.4
      LViewGroup := LMainActivity.getContentView;
      {$ELSE}
      LViewGroup := LMainActivity.getViewGroup;
      {$ENDIF}
      if LViewGroup <> nil then
      begin
        LWindowInsets := LViewGroup.getRootWindowInsets;
        if LWindowInsets <> nil then
        begin
          if TOSVersion.Check(11) then // Android 11 (Api level 30) or later
          begin
            LInsets := TAndroid11.TJWindowInsetsEx.Wrap((LWindowInsets as ILocalObject).GetObjectID).getInsets(TAndroid11.TJWindowInsets_Type.JavaClass.systemBars);
            if LInsets <> nil then
              Result := TRect.Create(LInsets.left, LInsets.top, LInsets.right, LInsets.bottom);
          end
          else
            Result := TRect.Create(LWindowInsets.getSystemWindowInsetLeft,
              LWindowInsets.getSystemWindowInsetTop,
              LWindowInsets.getSystemWindowInsetRight,
              LWindowInsets.getSystemWindowInsetBottom);
        end;
      end;
    end;
  end
  {$IF CompilerVersion < 34.0} // Delphi Sydney 10.4
  else if TOSVersion.Check(5) or (TJBuild_VERSION.JavaClass.SDK_INT >= 19) then // Android 4.4 (Kitkat / Api level 19) or later
    Result := TAndroidBeforeMarshmallow.AbsoluteSystemInsets;
  {$ENDIF}
end;

class function TAndroidSystemBars.AbsoluteTappableInsets: TRect;
var
  LMainActivity: JFMXNativeActivity;
  LViewGroup: JViewGroup;
  LInsets: TAndroid10.JInsets;
  LObject: JObject;
begin
  if TOSVersion.Check(10) then // Android 10 (Api level 29) or later
  begin
    Result := TRect.Empty;
    LMainActivity := MainActivity;
    if LMainActivity <> nil then
    begin
      {$IF CompilerVersion >= 34.0} // Delphi Sydney 10.4
      LViewGroup := LMainActivity.getContentView;
      {$ELSE}
      LViewGroup := LMainActivity.getViewGroup;
      {$ENDIF}
      if LViewGroup <> nil then
      begin
        LObject := LViewGroup.getRootWindowInsets;
        if LObject <> nil then
        begin
          if TOSVersion.Check(11) then // Android 11 (Api level 30) or later
            LInsets := TAndroid11.TJWindowInsetsEx.Wrap((LObject as ILocalObject).GetObjectID).getInsets(TAndroid11.TJWindowInsets_Type.JavaClass.tappableElement)
          else
            LInsets := TAndroid10.TJWindowInsetsEx.Wrap((LObject as ILocalObject).GetObjectID).getTappableElementInsets;
          if LInsets <> nil then
            Result := TRect.Create(LInsets.left, LInsets.top, LInsets.right, LInsets.bottom);
        end;
      end;
    end;
  end
  else
    Result := AbsoluteSystemInsets;
end;

class function TAndroidSystemBars.AbsoluteToScaled(const AAbsoluteRect: TRect): TRectF;

  function GetScreenScale: Single;
  var
    LScreenService: IFMXScreenService;
  begin
    if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, LScreenService) then
      Result := LScreenService.GetScreenScale
    else
      Result := 1;
  end;

var
  LEpsilonPositionRange: Integer;
  LScreenScale: Single;
begin
  LScreenScale := GetScreenScale;
  LEpsilonPositionRange := -3;
  Assert(LEpsilonPositionRange = Round(Log10(TEpsilon.Position)));
  Result := TRectF.Create(RoundTo(AAbsoluteRect.Left / LScreenScale, LEpsilonPositionRange),
    RoundTo(AAbsoluteRect.Top / LScreenScale, LEpsilonPositionRange),
    RoundTo(AAbsoluteRect.Right / LScreenScale, LEpsilonPositionRange),
    RoundTo(AAbsoluteRect.Bottom / LScreenScale, LEpsilonPositionRange));
end;

class constructor TAndroidSystemBars.Create;
begin
  FInsetsChangeMessageId := TMessageManager.DefaultManager.SubscribeToMessage(TAndroidInsetsChangeMessage, InsetsChangeHandler);
  FMainFormChangedMessageId := TMessageManager.DefaultManager.SubscribeToMessage(TMainFormChangedMessage, MainFormChangedHandler);
end;

class destructor TAndroidSystemBars.Destroy;
var
  LMainActivity: JFMXNativeActivity;
  LViewGroup: JViewGroup;
begin
  TMessageManager.DefaultManager.Unsubscribe(TMainFormChangedMessage, FMainFormChangedMessageId);
  TMessageManager.DefaultManager.Unsubscribe(TAndroidInsetsChangeMessage, FInsetsChangeMessageId);
  if Assigned(FListener) then
  begin
    LMainActivity := MainActivity;
    if LMainActivity <> nil then
    begin
      {$IF CompilerVersion >= 34.0} // Delphi Sydney 10.4
      LViewGroup := LMainActivity.getContentView;
      {$ELSE}
      LViewGroup := LMainActivity.getViewGroup;
      {$ENDIF}
      if LViewGroup <> nil then
        LViewGroup.setOnApplyWindowInsetsListener(nil);
    end;
    FListener.Free;
  end;
end;

class procedure TAndroidSystemBars.InsetsChangeHandler(const ASender: TObject;
  const AMessage: System.Messaging.TMessage);
begin
  if Assigned(FOnInsetsChange) then
    FOnInsetsChange(nil);
end;

class procedure TAndroidSystemBars.MainFormChangedHandler(const ASender: TObject;
  const AMessage: System.Messaging.TMessage);
var
  LMainActivity: JFMXNativeActivity;
  LViewGroup: JViewGroup;
begin
  if FInitialized then
    Exit;
  FInitialized := True;
  LMainActivity := MainActivity;
  if LMainActivity <> nil then
  begin
    {$IF CompilerVersion >= 34.0} // Delphi Sydney 10.4
    LViewGroup := LMainActivity.getContentView;
    {$ELSE}
    LViewGroup := LMainActivity.getViewGroup;
    {$ENDIF}
    if LViewGroup <> nil then
    begin
      FListener := TOnApplyWindowInsetsListener.Create;
      LViewGroup.setOnApplyWindowInsetsListener(FListener);
    end;
  end;
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
// 2) you need to call SystemInsets or TappableInsets to get the sizes of system
// bars to set the padding in your form to avoid some controls like TEdit or
// TButton to go below the system bar
// Tested ides: Delphi Sydney 10.4.1 and Delphi Rio 10.3.3
// Tested devices:
// LG       - LM-X430 Android32 10.0.0 (API level 29)
// SAMSUNG  - SM-A013M Android32 10.0.0 (API level 29)
// SAMSUNG  - SM-G955F Android64 9.0.0 (API level 28)
// SAMSUNG  - SM-G935F Android64 8.0.0 (API level 26)
// MOTOROLA - MotoG3 Android32 6.0.0 (API level 23)
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
  LMainActivity: JFMXNativeActivity;
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
  LMainActivity := MainActivity;
  if LMainActivity <> nil then
    {$IF CompilerVersion >= 34.0} // Delphi Sydney 10.4
    LView := LMainActivity.getContentView
    {$ELSE}
    LView := LMainActivity.getViewGroup
    {$ENDIF}
  else
    LView := nil;
  if Assigned(LWindow) then
    LWinParams := LWindow.getAttributes
  else
    LWinParams := nil;

  // Setting the configurations
  if TOSVersion.Check(6) then // Android 6 (Marshmallow / Api level 23) or later
  begin
    if LStatusBarLight then
      LSystemUiVisibility := LSystemUiVisibility or TJView.JavaClass.SYSTEM_UI_FLAG_LIGHT_STATUS_BAR;
    LStatusBarJColor := TJColor.JavaClass.TRANSPARENT;
  end;
  if TOSVersion.Check(8) then // Android 8 (Oreo / Api level 26) or later
  begin
    if LNavigationBarLight then
      LSystemUiVisibility := LSystemUiVisibility or TJView.JavaClass.SYSTEM_UI_FLAG_LIGHT_NAVIGATION_BAR;
    // This is really necessary because some devices (in my tests, in device LG LM-X430 Android32 10.0.0 (API level 29),
    // the full transparent don't work, the transparent color is replaced by a semitransparent white. However, when we avoid
    // the full transparent color, it work in all devices)
    TAlphaColorRec(ANearNavigationBarColor).A := 1;
    LNavigationBarJColor := AlphaColorToJColor(ANearNavigationBarColor);
  end;
  if TOSVersion.Check(5) or (TJBuild_VERSION.JavaClass.SDK_INT >= 19) then // Android 4.4 (Kitkat / Api level 19) or later
  begin
    LSystemUiVisibility := LSystemUiVisibility or
      TJView.JavaClass.SYSTEM_UI_FLAG_LAYOUT_STABLE or
      TJView.JavaClass.SYSTEM_UI_FLAG_LAYOUT_FULLSCREEN or
      TJView.JavaClass.SYSTEM_UI_FLAG_LAYOUT_HIDE_NAVIGATION;
    if Assigned(LView) then
      LView.setSystemUiVisibility(LSystemUiVisibility);
  end;
  if (not TOSVersion.Check(5)) and (TJBuild_VERSION.JavaClass.SDK_INT >= 19) and
    Assigned(LWinParams) then // Android 4.4 (Kitkat / Api level 19) and Android 4.4.4 (Kitkat / Api level 20)
  begin
    LWinParams.flags := LWinParams.flags or
      TJWindowManager_LayoutParams.JavaClass.FLAG_TRANSLUCENT_STATUS or
      TJWindowManager_LayoutParams.JavaClass.FLAG_TRANSLUCENT_NAVIGATION;
  end;
  if TOSVersion.Check(5) then // Android 5 (Lollipop / Api level 21) or later
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

class function TAndroidSystemBars.SystemInsets: TRectF;
begin
  Result := AbsoluteToScaled(AbsoluteSystemInsets);
end;

class function TAndroidSystemBars.TappableInsets: TRectF;
begin
  Result := AbsoluteToScaled(AbsoluteTappableInsets);
end;

initialization
  TRegTypes.RegisterType('iPub.Android.SystemBars.TAndroid10.JInsets', TypeInfo(iPub.Android.SystemBars.TAndroid10.JInsets));
  TRegTypes.RegisterType('iPub.Android.SystemBars.TAndroid11.JWindowInsets_Type', TypeInfo(iPub.Android.SystemBars.TAndroid11.JWindowInsets_Type));
{$ELSE}
implementation
{$ENDIF}
end.
