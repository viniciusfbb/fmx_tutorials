{************************************************************************}
{                                                                        }
{                          iPub.FMX.SystemBars                           }
{                                                                        }
{ Copyright (c) 2021-2022 iPub                                           }
{ https://github.com/viniciusfbb/fmx_tutorials                           }
{                                                                        }
{ Use of this source code is governed by a MIT license that can be found }
{ at https://opensource.org/licenses/MIT                                 }
{                                                                        }
{************************************************************************}
unit iPub.FMX.SystemBars.Android;

interface

{$SCOPEDENUMS ON}
{$IFDEF ANDROID}

implementation

uses
  { Delphi }
  System.Classes,
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Messaging,
  System.Math,
  System.Math.Vectors,
  System.Generics.Collections,
  FMX.Types,
  FMX.Utils,
  FMX.Forms,
  FMX.Platform,
  FMX.Platform.Android,
  FMX.Platform.UI.Android,
  FMX.Platform.Screen.Android,
  Androidapi.JNIBridge,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.Helpers,
  Androidapi.Jni,
  Androidapi.JNI.Embarcadero,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Os,
  Androidapi.JNI.App,
  Androidapi.JNI.Widget,

  { iPub }
  iPub.FMX.SystemBars;

type
  { IipSystemBarsServiceAndroid }

  IipSystemBarsServiceAndroid = interface(TipFormSystemBars.IFMXWindowSystemBarsService)
    ['{77586947-BF49-4938-9A34-51588E8BD915}']
    procedure CheckInsetsChanges(const AForm: TCommonCustomForm);
    function HasGestureNavigationBar(const AForm: TCommonCustomForm): Boolean;
    procedure TryFixInvisibleMode;
  end;

  { TipSystemBarsServiceAndroid }

  TipSystemBarsServiceAndroid = class(TInterfacedObject, IipSystemBarsServiceAndroid,
    TipFormSystemBars.IFMXWindowSystemBarsService, IFMXWindowSystemStatusBarService,
    IFMXFullScreenWindowService)
  strict private
    type
      TOnApplyWindowInsetsListener = class(TJavaLocal, JView_OnApplyWindowInsetsListener)
      strict private
        FChangeChecksEnabled: Boolean;
      public
        constructor Create;
        function onApplyWindowInsets(v: JView; insets: JWindowInsets): JWindowInsets; cdecl;
        property ChangeChecksEnabled: Boolean read FChangeChecksEnabled write FChangeChecksEnabled;
      end;

      TOnAttachStateChangeListener = class(TJavaLocal, JView_OnAttachStateChangeListener)
      public
        procedure onViewAttachedToWindow(v: JView); cdecl;
        procedure onViewDetachedFromWindow(v: JView); cdecl;
      end;

      TOnWindowFocusChangeListener = class(TJavaLocal, JViewTreeObserver_OnWindowFocusChangeListener)
      public
        procedure onWindowFocusChanged(hasFocus: Boolean); cdecl;
      end;

      TOnTouchListener = class(TJavaLocal, JView_OnTouchListener)
      public
        function onTouch(v: JView; event: JMotionEvent): Boolean; cdecl;
      end;

      TWindowServiceFix = class(TInterfacedObject, IFMXWindowService)
      strict private
        FDefaultWindowService: IFMXWindowService;
      public
        constructor Create;
        destructor Destroy; override;
        { IFMXWindowService }
        function FindForm(const AHandle: TWindowHandle): TCommonCustomForm;
        function CreateWindow(const AForm: TCommonCustomForm): TWindowHandle;
        procedure DestroyWindow(const AForm: TCommonCustomForm);
        procedure ReleaseWindow(const AForm: TCommonCustomForm);
        procedure SetWindowState(const AForm: TCommonCustomForm; const AState: TWindowState);
        procedure ShowWindow(const AForm: TCommonCustomForm);
        procedure HideWindow(const AForm: TCommonCustomForm);
        function ShowWindowModal(const AForm: TCommonCustomForm): TModalResult;
        procedure InvalidateWindowRect(const AForm: TCommonCustomForm; R: TRectF);
        procedure InvalidateImmediately(const AForm: TCommonCustomForm);
        procedure SetWindowRect(const AForm: TCommonCustomForm; ARect: TRectF);
        function GetWindowRect(const AForm: TCommonCustomForm): TRectF;
        function GetClientSize(const AForm: TCommonCustomForm): TPointF;
        procedure SetClientSize(const AForm: TCommonCustomForm; const ASize: TPointF);
        procedure SetWindowCaption(const AForm: TCommonCustomForm; const ACaption: string);
        procedure SetCapture(const AForm: TCommonCustomForm);
        procedure ReleaseCapture(const AForm: TCommonCustomForm);
        function ClientToScreen(const AForm: TCommonCustomForm; const Point: TPointF): TPointF;
        function ScreenToClient(const AForm: TCommonCustomForm; const Point: TPointF): TPointF;
        procedure BringToFront(const AForm: TCommonCustomForm);
        procedure SendToBack(const AForm: TCommonCustomForm);
        procedure Activate(const AForm: TCommonCustomForm);
        function GetWindowScale(const AForm: TCommonCustomForm): Single;
        function CanShowModal: Boolean;
      end;
  strict private
    FAfterCreateFormHandleMessageId: Integer;
    FBeforeDestroyFormHandleMessageId: Integer;
    FDefaultFullScreenService: IFMXFullScreenWindowService;
    FFormActivateMessageId: Integer;
    FFormReleasedMessageId: Integer;
    FDefaultFormVisibility: TDictionary<TCommonCustomForm, TipFormSystemBars.TVisibilityMode>;
    FOnApplyWindowInsetsListener: TOnApplyWindowInsetsListener;
    FOnAttachStateChangeListener: TOnAttachStateChangeListener;
    FOnTouchListener: TOnTouchListener;
    FOnWindowFocusChangeListener: TOnWindowFocusChangeListener;
    FRegisteredBarsService: Boolean;
    FRegisteredStatusBarService: Boolean;
    FVirtualKeyboardBounds: TRect;
    FVirtualKeyboardMessageId: Integer;
    FWindowServiceFix: IFMXWindowService;
    class function AbsoluteRectToScreenScaled(const AAbsoluteRect: TRect): TRectF; static;
    procedure AfterCreateFormHandle(const ASender: TObject; const AMessage: TMessage);
    procedure ApplicationEventHandler(const ASender: TObject; const AMessage: TMessage);
    procedure BeforeDestroyFormHandle(const ASender: TObject; const AMessage: TMessage);
    function CanFormChangeSystemBars(const AForm: TCommonCustomForm): Boolean;
    procedure CheckInsetsChanges(const AForm: TCommonCustomForm);
    function DoGetAbsoluteInsets(const AForm: TCommonCustomForm): TRect;
    function DoGetAbsoluteTappableInsets(const AForm: TCommonCustomForm): TRect;
    procedure DoSetVisibility(const AForm: TCommonCustomForm; const AMode: TipFormSystemBars.TVisibilityMode);
    procedure FormActivate(const ASender: TObject; const AMessage: TMessage);
    procedure FormReleased(const ASender: TObject; const AMessage: TMessage);
    function GetAbsoluteInsets(const AForm: TCommonCustomForm): TRect;
    function GetAbsoluteTappableInsets(const AForm: TCommonCustomForm): TRect;
    class function GetMainActivityContentView: JViewGroup; static;
    class function GetWindowDecorView: JView; static;
    function HasGestureNavigationBar(const AForm: TCommonCustomForm): Boolean;
    function HasSystemBars(const AForm: TCommonCustomForm): Boolean;
    function IsDarkTheme: Boolean;
    procedure RefreshView(const AView: JView);
    function RemoveKeyboardOverlappedBars(const AInsets: TRectF; const AForm: TCommonCustomForm): TRectF;
    procedure TryFixInvisibleMode;
    procedure VirtualKeyboardChangeHandler(const ASender: TObject; const AMessage: System.Messaging.TMessage);
  public
    constructor Create;
    destructor Destroy; override;
    { IFMXWindowSystemBarsService }
    function GetInsets(const AForm: TCommonCustomForm): TRectF;
    function GetTappableInsets(const AForm: TCommonCustomForm): TRectF;
    procedure SetNavigationBarBackgroundColor(const AForm: TCommonCustomForm; const AColor: TAlphaColor);
    { IFMXWindowSystemStatusBarService / IFMXWindowSystemBarsService }
    procedure IFMXWindowSystemStatusBarService.SetBackgroundColor = SetStatusBarBackgroundColor;
    procedure SetStatusBarBackgroundColor(const AForm: TCommonCustomForm; const AColor: TAlphaColor);
    procedure SetVisibility(const AForm: TCommonCustomForm; const AMode: TipFormSystemBars.TVisibilityMode);
    { IFMXFullScreenWindowService }
    function GetFullScreen(const AForm: TCommonCustomForm): Boolean;
    procedure SetFullScreen(const AForm: TCommonCustomForm; const AValue: Boolean);
    procedure SetShowFullScreenIcon(const AForm: TCommonCustomForm; const AValue: Boolean);
  end;

  { TAndroid9 }

  TAndroid9 = class
  public type
    { WindowManager.LayoutParams }

    JWindowManager_LayoutParamsClass = interface(Androidapi.JNI.GraphicsContentViewText.JWindowManager_LayoutParamsClass)
      ['{D657960B-0CEA-4A56-9FC1-EB165CCB4891}']
      {class} function _GetLAYOUT_IN_DISPLAY_CUTOUT_MODE_DEFAULT: Integer; cdecl;
      {class} function _GetLAYOUT_IN_DISPLAY_CUTOUT_MODE_SHORT_EDGES: Integer; cdecl;
      {class} property LAYOUT_IN_DISPLAY_CUTOUT_MODE_DEFAULT: Integer read _GetLAYOUT_IN_DISPLAY_CUTOUT_MODE_DEFAULT;
      {class} property LAYOUT_IN_DISPLAY_CUTOUT_MODE_SHORT_EDGES: Integer read _GetLAYOUT_IN_DISPLAY_CUTOUT_MODE_SHORT_EDGES;
    end;

    [JavaSignature('android/view/WindowManager$LayoutParams')]
    JWindowManager_LayoutParams = interface(Androidapi.JNI.GraphicsContentViewText.JWindowManager_LayoutParams)
      ['{C849F24F-5CAC-425E-B5B0-0EE72E71966B}']
      function _GetlayoutInDisplayCutoutMode: Integer; cdecl;
      procedure _SetlayoutInDisplayCutoutMode(Value: Integer); cdecl;
      property layoutInDisplayCutoutMode: Integer read _GetlayoutInDisplayCutoutMode write _SetlayoutInDisplayCutoutMode;
    end;
    TJWindowManager_LayoutParams = class(TJavaGenericImport<TAndroid9.JWindowManager_LayoutParamsClass, TAndroid9.JWindowManager_LayoutParams>) end;
  end;

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
    TJInsets = class(TJavaGenericImport<TAndroid10.JInsetsClass, TAndroid10.JInsets>) end;


    { WindowInsets }

    JWindowInsetsClass = interface(Androidapi.JNI.GraphicsContentViewText.JWindowInsetsClass)
      ['{6BCCFCCA-A7F0-4740-B4D6-D03286DAD89C}']
    end;

    [JavaSignature('android/view/WindowInsets')]
    JWindowInsets = interface(Androidapi.JNI.GraphicsContentViewText.JWindowInsets)
      ['{05AFC51B-3683-4DD1-BB1A-22799899142B}']
      function getMandatorySystemGestureInsets: JInsets; cdecl;
      function getTappableElementInsets: JInsets; cdecl;
    end;
    TJWindowInsets = class(TJavaGenericImport<TAndroid10.JWindowInsetsClass, TAndroid10.JWindowInsets>) end;


    { Window }

    JWindowClass = interface(Androidapi.JNI.GraphicsContentViewText.JWindowClass)
      ['{FE27644B-35A7-48EA-90A3-42CC8CB5E1B9}']
    end;

    [JavaSignature('android/view/Window')]
    JWindow = interface(Androidapi.JNI.GraphicsContentViewText.JWindow)
      ['{25E670D9-5AFB-4C61-9703-D5E5CD89F66F}']
      procedure setNavigationBarContrastEnforced(enforceContrast: Boolean); cdecl;
      procedure setStatusBarContrastEnforced(enforceContrast: Boolean); cdecl;
    end;
    TJWindow = class(TJavaGenericImport<TAndroid10.JWindowClass, TAndroid10.JWindow>) end;
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
    TJWindowInsets_Type = class(TJavaGenericImport<TAndroid11.JWindowInsets_TypeClass, TAndroid11.JWindowInsets_Type>) end;


    { WindowInsetsController }

    JWindowInsetsControllerClass = interface(JObjectClass)
      ['{D7FCB1D1-65AD-4A41-B873-6F8CD3B1E6F4}']
      {class} function _GetAPPEARANCE_LIGHT_NAVIGATION_BARS: Integer; cdecl;
      {class} function _GetAPPEARANCE_LIGHT_STATUS_BARS: Integer; cdecl;
      {class} function _GetBEHAVIOR_SHOW_BARS_BY_SWIPE: Integer; cdecl;
      {class} function _GetBEHAVIOR_SHOW_BARS_BY_TOUCH: Integer; cdecl;
      {class} function _GetBEHAVIOR_SHOW_TRANSIENT_BARS_BY_SWIPE: Integer; cdecl;
      {class} property APPEARANCE_LIGHT_NAVIGATION_BARS: Integer read _GetAPPEARANCE_LIGHT_NAVIGATION_BARS;
      {class} property APPEARANCE_LIGHT_STATUS_BARS: Integer read _GetAPPEARANCE_LIGHT_STATUS_BARS;
      {class} property BEHAVIOR_SHOW_BARS_BY_SWIPE: Integer read _GetBEHAVIOR_SHOW_BARS_BY_SWIPE;
      {class} property BEHAVIOR_SHOW_BARS_BY_TOUCH: Integer read _GetBEHAVIOR_SHOW_BARS_BY_TOUCH;
      {class} property BEHAVIOR_SHOW_TRANSIENT_BARS_BY_SWIPE: Integer read _GetBEHAVIOR_SHOW_TRANSIENT_BARS_BY_SWIPE;
    end;

    [JavaSignature('android/view/WindowInsetsController')]
    JWindowInsetsController = interface(JObject)
      ['{895DBAB1-0A52-4240-AC13-ED40474E6850}']
      function getSystemBarsAppearance: Integer; cdecl;
      function getSystemBarsBehavior: Integer; cdecl;
      procedure hide(types: Integer); cdecl;
      procedure setSystemBarsAppearance(appearance, mask: Integer); cdecl;
      procedure setSystemBarsBehavior(behavior: Integer); cdecl;
      procedure show(types: Integer); cdecl;
    end;
    TJWindowInsetsController = class(TJavaGenericImport<TAndroid11.JWindowInsetsControllerClass, TAndroid11.JWindowInsetsController>) end;


    { WindowInsets }

    JWindowInsetsClass = interface(Androidapi.JNI.GraphicsContentViewText.JWindowInsetsClass)
      ['{6BCCFCCA-A7F0-4740-B4D6-D03286DAD89C}']
    end;

    [JavaSignature('android/view/WindowInsets')]
    JWindowInsets = interface(Androidapi.JNI.GraphicsContentViewText.JWindowInsets)
      ['{05AFC51B-3683-4DD1-BB1A-22799899142B}']
      function getInsets(typeMask: Integer): TAndroid10.JInsets; cdecl;
      function isVisible(typeMask: Integer): Boolean; cdecl;
    end;
    TJWindowInsets = class(TJavaGenericImport<TAndroid11.JWindowInsetsClass, TAndroid11.JWindowInsets>) end;


    { Window }

    JWindowClass = interface(Androidapi.JNI.GraphicsContentViewText.JWindowClass)
      ['{6A055684-AA34-432C-8B18-12B7D721C599}']
    end;

    [JavaSignature('android/view/Window')]
    JWindow = interface(Androidapi.JNI.GraphicsContentViewText.JWindow)
      ['{BC863876-71E3-4292-8B2C-860FADB8066C}']
      procedure setDecorFitsSystemWindows(decorFitsSystemWindows: Boolean); cdecl;
    end;
    TJWindowEx = class(TJavaGenericImport<TAndroid11.JWindowClass, TAndroid11.JWindow>) end;


    { View }

    JViewClass = interface(Androidapi.JNI.GraphicsContentViewText.JViewClass)
      ['{44C48716-DA19-42A7-9141-E137DC92598F}']
    end;

    [JavaSignature('android/view/View')]
    JView = interface(Androidapi.JNI.GraphicsContentViewText.JView)
      ['{150FC57B-6421-4F25-A626-D0B51AA9E4FF}']
      function getWindowInsetsController: JWindowInsetsController; cdecl;
    end;
    TJView = class(TJavaGenericImport<TAndroid11.JViewClass, TAndroid11.JView>) end;
  end;

{$REGION 'Backwards compatibility'}
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
{$ENDREGION}

var
  FSystemBarsServiceAndroid: IipSystemBarsServiceAndroid;

{ TipSystemBarsServiceAndroid.TOnApplyWindowInsetsListener }

constructor TipSystemBarsServiceAndroid.TOnApplyWindowInsetsListener.Create;
begin
  inherited Create;
  FChangeChecksEnabled := True;
end;

function TipSystemBarsServiceAndroid.TOnApplyWindowInsetsListener.onApplyWindowInsets(v: JView;
  insets: JWindowInsets): JWindowInsets;
var
  LForm: TCommonCustomForm;
begin
  Result := v.OnApplyWindowInsets(insets);
  if FChangeChecksEnabled and Assigned(FSystemBarsServiceAndroid) and Assigned(Screen) then
  begin
    LForm := Screen.ActiveForm;
    if Assigned(LForm) then
      FSystemBarsServiceAndroid.CheckInsetsChanges(LForm);
  end;
end;

{ TipSystemBarsServiceAndroid.TOnAttachStateChangeListener }

procedure TipSystemBarsServiceAndroid.TOnAttachStateChangeListener.onViewAttachedToWindow(
  v: JView);
var
  LForm: TCommonCustomForm;
  LFormSystemBars: TipFormSystemBars;
begin
  // In first FormActive, the MainActivity.getContentView is not attached to window, so we
  // need to set the configs again to fix any problem in first SetVisibility (like insets calculations)
  if Assigned(FSystemBarsServiceAndroid) and Assigned(Screen) then
  begin
    LForm := Screen.ActiveForm;
    if Assigned(LForm) then
    begin
      LFormSystemBars := LForm.SystemBars;
      if Assigned(LFormSystemBars) then
        FSystemBarsServiceAndroid.SetVisibility(LForm, LFormSystemBars.Visibility);
    end;
  end;
end;

procedure TipSystemBarsServiceAndroid.TOnAttachStateChangeListener.onViewDetachedFromWindow(
  v: JView);
begin
end;

{ TipSystemBarsServiceAndroid.TOnWindowFocusChangeListener }

procedure TipSystemBarsServiceAndroid.TOnWindowFocusChangeListener.onWindowFocusChanged(
  hasFocus: Boolean);
begin
  if hasFocus and Assigned(FSystemBarsServiceAndroid) then
    FSystemBarsServiceAndroid.TryFixInvisibleMode;
end;

{ TipSystemBarsServiceAndroid.TOnTouchListener }

function TipSystemBarsServiceAndroid.TOnTouchListener.onTouch(v: JView;
  event: JMotionEvent): Boolean;
var
  LForm: TCommonCustomForm;
  LFormSystemBars: TipFormSystemBars;
  LTouchPoint: TPointF;
  LBottomInsets: Single;
begin
  Result := False;
  if TOSVersion.Check(10) and // Android 10 (api level 29) or later
    Assigned(event) and (event.getAction = TJMotionEvent.JavaClass.ACTION_DOWN) then
  begin
    // Remove touch events from system gesture bar area, for touch processing in this area
    // be exclusive to the operating system, as in iOS. It is better this way because the double
    // processing generates some inconveniences like scrolling along with the form close gesture
    if Assigned(FSystemBarsServiceAndroid) and Assigned(Screen) then
    begin
      LForm := Screen.ActiveForm;
      if Assigned(LForm) then
      begin
        LFormSystemBars := LForm.SystemBars;
        if Assigned(LFormSystemBars) and (LFormSystemBars.Visibility <> TipFormSystemBars.TVisibilityMode.Invisible) then
        begin
          LBottomInsets := LFormSystemBars.Insets.Bottom;
          if (LBottomInsets > 0) and FSystemBarsServiceAndroid.HasGestureNavigationBar(LForm) then
          begin
            LTouchPoint := ConvertPixelToPoint(TPointF.Create(event.getRawX, event.getRawY));
            LTouchPoint := LForm.ScreenToClient(LTouchPoint);
            Result := LTouchPoint.Y > LForm.Height - LBottomInsets;
          end;
        end;
      end;
    end;
  end;
end;

{ TipSystemBarsServiceAndroid.TWindowServiceFix }

// This class is just to fix the ClientToScreen and ScreenToClient in VisibleAndOverlap mode

procedure TipSystemBarsServiceAndroid.TWindowServiceFix.Activate(
  const AForm: TCommonCustomForm);
begin
  FDefaultWindowService.Activate(AForm);
end;

procedure TipSystemBarsServiceAndroid.TWindowServiceFix.BringToFront(
  const AForm: TCommonCustomForm);
begin
  FDefaultWindowService.BringToFront(AForm);
end;

function TipSystemBarsServiceAndroid.TWindowServiceFix.CanShowModal: Boolean;
begin
  Result := FDefaultWindowService.CanShowModal;
end;

function TipSystemBarsServiceAndroid.TWindowServiceFix.ClientToScreen(
  const AForm: TCommonCustomForm; const Point: TPointF): TPointF;
var
  LSystemBars: TipFormSystemBars;
begin
  Result := FDefaultWindowService.ClientToScreen(AForm, Point);
  LSystemBars := AForm.SystemBars;
  if Assigned(LSystemBars) and (LSystemBars.Visibility = TipFormSystemBars.TVisibilityMode.VisibleAndOverlap) then
    Result.Y := Result.Y - LSystemBars.TappableInsets.Top;
end;

constructor TipSystemBarsServiceAndroid.TWindowServiceFix.Create;
begin
  inherited Create;
  { IFMXFullScreenWindowService }
  if TPlatformServices.Current.SupportsPlatformService(IFMXWindowService, FDefaultWindowService) then
  begin
    TPlatformServices.Current.RemovePlatformService(IFMXWindowService);
    TPlatformServices.Current.AddPlatformService(IFMXWindowService, Self);
  end;
end;

function TipSystemBarsServiceAndroid.TWindowServiceFix.CreateWindow(
  const AForm: TCommonCustomForm): TWindowHandle;
begin
  Result := FDefaultWindowService.CreateWindow(AForm);
end;

destructor TipSystemBarsServiceAndroid.TWindowServiceFix.Destroy;
begin
  { IFMXFullScreenWindowService }
  TPlatformServices.Current.RemovePlatformService(IFMXWindowService);
  if Assigned(FDefaultWindowService) then
    TPlatformServices.Current.AddPlatformService(IFMXWindowService, FDefaultWindowService);
  inherited;
end;

procedure TipSystemBarsServiceAndroid.TWindowServiceFix.DestroyWindow(
  const AForm: TCommonCustomForm);
begin
  FDefaultWindowService.DestroyWindow(AForm);
end;

function TipSystemBarsServiceAndroid.TWindowServiceFix.FindForm(
  const AHandle: TWindowHandle): TCommonCustomForm;
begin
  Result := FDefaultWindowService.FindForm(AHandle);
end;

function TipSystemBarsServiceAndroid.TWindowServiceFix.GetClientSize(
  const AForm: TCommonCustomForm): TPointF;
begin
  Result := FDefaultWindowService.GetClientSize(AForm);
end;

function TipSystemBarsServiceAndroid.TWindowServiceFix.GetWindowRect(
  const AForm: TCommonCustomForm): TRectF;
begin
  Result := FDefaultWindowService.GetWindowRect(AForm);
end;

function TipSystemBarsServiceAndroid.TWindowServiceFix.GetWindowScale(
  const AForm: TCommonCustomForm): Single;
begin
  Result := TWindowServiceAndroid(TObject(FDefaultWindowService)).GetWindowScale(AForm);
end;

procedure TipSystemBarsServiceAndroid.TWindowServiceFix.HideWindow(
  const AForm: TCommonCustomForm);
begin
  FDefaultWindowService.HideWindow(AForm);
end;

procedure TipSystemBarsServiceAndroid.TWindowServiceFix.InvalidateImmediately(
  const AForm: TCommonCustomForm);
begin
  FDefaultWindowService.InvalidateImmediately(AForm);
end;

procedure TipSystemBarsServiceAndroid.TWindowServiceFix.InvalidateWindowRect(
  const AForm: TCommonCustomForm; R: TRectF);
begin
  FDefaultWindowService.InvalidateWindowRect(AForm, R);
end;

procedure TipSystemBarsServiceAndroid.TWindowServiceFix.ReleaseCapture(
  const AForm: TCommonCustomForm);
begin
  FDefaultWindowService.ReleaseCapture(AForm);
end;

procedure TipSystemBarsServiceAndroid.TWindowServiceFix.ReleaseWindow(
  const AForm: TCommonCustomForm);
begin
  FDefaultWindowService.ReleaseWindow(AForm);
end;

function TipSystemBarsServiceAndroid.TWindowServiceFix.ScreenToClient(
  const AForm: TCommonCustomForm; const Point: TPointF): TPointF;
var
  LSystemBars: TipFormSystemBars;
begin
  Result := FDefaultWindowService.ScreenToClient(AForm, Point);
  LSystemBars := AForm.SystemBars;
  if Assigned(LSystemBars) and (LSystemBars.Visibility = TipFormSystemBars.TVisibilityMode.VisibleAndOverlap) then
    Result.Y := Result.Y + LSystemBars.TappableInsets.Top;
end;

procedure TipSystemBarsServiceAndroid.TWindowServiceFix.SendToBack(
  const AForm: TCommonCustomForm);
begin
  FDefaultWindowService.SendToBack(AForm);
end;

procedure TipSystemBarsServiceAndroid.TWindowServiceFix.SetCapture(
  const AForm: TCommonCustomForm);
begin
  FDefaultWindowService.SetCapture(AForm);
end;

procedure TipSystemBarsServiceAndroid.TWindowServiceFix.SetClientSize(
  const AForm: TCommonCustomForm; const ASize: TPointF);
begin
  FDefaultWindowService.SetClientSize(AForm, ASize);
end;

procedure TipSystemBarsServiceAndroid.TWindowServiceFix.SetWindowCaption(
  const AForm: TCommonCustomForm; const ACaption: string);
begin
  FDefaultWindowService.SetWindowCaption(AForm, ACaption);
end;

procedure TipSystemBarsServiceAndroid.TWindowServiceFix.SetWindowRect(
  const AForm: TCommonCustomForm; ARect: TRectF);
begin
  FDefaultWindowService.SetWindowRect(AForm, ARect);
end;

procedure TipSystemBarsServiceAndroid.TWindowServiceFix.SetWindowState(
  const AForm: TCommonCustomForm; const AState: TWindowState);
begin
  FDefaultWindowService.SetWindowState(AForm, AState);
end;

procedure TipSystemBarsServiceAndroid.TWindowServiceFix.ShowWindow(
  const AForm: TCommonCustomForm);
begin
  FDefaultWindowService.ShowWindow(AForm);
end;

function TipSystemBarsServiceAndroid.TWindowServiceFix.ShowWindowModal(
  const AForm: TCommonCustomForm): TModalResult;
begin
  Result := ShowWindowModal(AForm);
end;

{ TipSystemBarsServiceAndroid }

class function TipSystemBarsServiceAndroid.AbsoluteRectToScreenScaled(
  const AAbsoluteRect: TRect): TRectF;
var
  LEpsilonPositionRange: Integer;
begin
  if AAbsoluteRect = TRect.Empty then
    Exit(TRectF.Empty);
  Result := TRectF.Create(ConvertPixelToPoint(AAbsoluteRect.TopLeft), ConvertPixelToPoint(AAbsoluteRect.BottomRight));
  // Round to position epsilon
  LEpsilonPositionRange := -3;
  Assert(LEpsilonPositionRange = Round(Log10(TEpsilon.Position)));
  Result := TRectF.Create(RoundTo(Result.Left, LEpsilonPositionRange),
    RoundTo(Result.Top, LEpsilonPositionRange),
    RoundTo(Result.Right, LEpsilonPositionRange),
    RoundTo(Result.Bottom, LEpsilonPositionRange));
end;

procedure TipSystemBarsServiceAndroid.AfterCreateFormHandle(
  const ASender: TObject; const AMessage: TMessage);

  procedure TryApplyInsetsListener;
  var
    LViewGroup: JViewGroup;
  begin
    LViewGroup := GetMainActivityContentView;
    if Assigned(LViewGroup) then
    begin
      {$IF CompilerVersion < 34.0} // Delphi Sydney 10.4
      if TJBuild_VERSION.JavaClass.SDK_INT >= 20 then // Android 4.4W (Kitkat Watch / api level 20) or later
      {$ENDIF}
      begin
        FOnApplyWindowInsetsListener := TOnApplyWindowInsetsListener.Create;
        LViewGroup.setOnApplyWindowInsetsListener(FOnApplyWindowInsetsListener);
      end;
      FOnAttachStateChangeListener := TOnAttachStateChangeListener.Create;
      LViewGroup.addOnAttachStateChangeListener(FOnAttachStateChangeListener);
      {$IF CompilerVersion < 34.0} // Delphi Sydney 10.4
      if TOSVersion.Check(4, 3) then // Android 4.3 (Jelly Bean / api level 18) or later
      {$ENDIF}
      begin
        FOnWindowFocusChangeListener := TOnWindowFocusChangeListener.Create;
        LViewGroup.getViewTreeObserver.addOnWindowFocusChangeListener(FOnWindowFocusChangeListener);
      end;
    end;
    if not Assigned(FOnTouchListener) then
      FOnTouchListener := TOnTouchListener.Create;
  end;

var
  LForm: TCommonCustomForm;
begin
  if not Assigned(FOnAttachStateChangeListener) then
    TryApplyInsetsListener;
  if ASender is TCommonCustomForm then
  begin
    LForm := TCommonCustomForm(ASender);
    if LForm.IsHandleAllocated then
      TAndroidWindowHandle(LForm.Handle).View.setOnTouchListener(FOnTouchListener);
    CheckInsetsChanges(LForm);
  end;
end;

procedure TipSystemBarsServiceAndroid.ApplicationEventHandler(
  const ASender: TObject; const AMessage: TMessage);
begin
  if (AMessage is TApplicationEventMessage) and (TApplicationEventMessage(AMessage).Value.Event = TApplicationEvent.BecameActive) then
  begin
    TThread.CreateAnonymousThread(
      procedure
      begin
        Sleep(1000);
        TThread.CurrentThread.ForceQueue(nil,
          procedure
          begin
            if Assigned(FSystemBarsServiceAndroid) then
              FSystemBarsServiceAndroid.TryFixInvisibleMode;
          end);
      end).Start;
  end;
end;

procedure TipSystemBarsServiceAndroid.BeforeDestroyFormHandle(
  const ASender: TObject; const AMessage: TMessage);
var
  LForm: TCommonCustomForm;
begin
  if ASender is TCommonCustomForm then
  begin
    LForm := TCommonCustomForm(ASender);
    if LForm.IsHandleAllocated then
      TAndroidWindowHandle(LForm.Handle).View.setOnTouchListener(nil);
  end;
end;

function TipSystemBarsServiceAndroid.CanFormChangeSystemBars(
  const AForm: TCommonCustomForm): Boolean;
begin
  Result := Assigned(AForm) and AForm.IsHandleAllocated and AForm.Active;
end;

procedure TipSystemBarsServiceAndroid.CheckInsetsChanges(
  const AForm: TCommonCustomForm);
var
  LNewInsets: TRectF;
  LNewTappableInsets: TRectF;
  LFormSystemBars: TipFormSystemBars;
begin
  if Assigned(AForm) and AForm.Active then
  begin
    LFormSystemBars := AForm.SystemBars;
    if Assigned(LFormSystemBars) then
    begin
      LNewInsets := GetInsets(AForm);
      LNewTappableInsets := GetTappableInsets(AForm);
      if (not LNewInsets.EqualsTo(LFormSystemBars.Insets, TEpsilon.Position)) or
        (not LNewTappableInsets.EqualsTo(LFormSystemBars.TappableInsets, TEpsilon.Position)) then
      begin
        TMessageManager.DefaultManager.SendMessage(AForm, TipFormSystemBars.TInsetsChangeMessage.Create(LNewInsets, LNewTappableInsets));
        TThread.ForceQueue(nil,
          procedure()
          begin
            if Assigned(FSystemBarsServiceAndroid) and Assigned(Screen) and (Screen.ActiveForm = AForm) then
              FSystemBarsServiceAndroid.SetVisibility(AForm, AForm.SystemBars.Visibility);
          end);
      end;
    end;
  end;
end;

constructor TipSystemBarsServiceAndroid.Create;
begin
  inherited;
  FWindowServiceFix := TWindowServiceFix.Create;
  { IFMXWindowSystemBarsService }
  if not TPlatformServices.Current.SupportsPlatformService(TipFormSystemBars.IFMXWindowSystemBarsService) then
  begin
    TPlatformServices.Current.AddPlatformService(TipFormSystemBars.IFMXWindowSystemBarsService, Self);
    FRegisteredBarsService := True;
  end;
  { IFMXWindowSystemStatusBarService }
  if not TPlatformServices.Current.SupportsPlatformService(IFMXWindowSystemStatusBarService) then
  begin
    TPlatformServices.Current.AddPlatformService(IFMXWindowSystemStatusBarService, Self);
    FRegisteredStatusBarService := True;
  end;
  { IFMXFullScreenWindowService }
  if TPlatformServices.Current.SupportsPlatformService(IFMXFullScreenWindowService, FDefaultFullScreenService) then
    TPlatformServices.Current.RemovePlatformService(IFMXFullScreenWindowService);
  TPlatformServices.Current.AddPlatformService(IFMXFullScreenWindowService, Self);

  FDefaultFormVisibility := TDictionary<TCommonCustomForm, TipFormSystemBars.TVisibilityMode>.Create;
  FAfterCreateFormHandleMessageId := TMessageManager.DefaultManager.SubscribeToMessage(TAfterCreateFormHandle, AfterCreateFormHandle);
  FBeforeDestroyFormHandleMessageId := TMessageManager.DefaultManager.SubscribeToMessage(TBeforeDestroyFormHandle, BeforeDestroyFormHandle);
  FFormActivateMessageId := TMessageManager.DefaultManager.SubscribeToMessage(TFormActivateMessage, FormActivate);
  FFormReleasedMessageId := TMessageManager.DefaultManager.SubscribeToMessage(TFormReleasedMessage, FormReleased);
  FVirtualKeyboardMessageId := TMessageManager.DefaultManager.SubscribeToMessage(TVKStateChangeMessage, VirtualKeyboardChangeHandler);
  TMessageManager.DefaultManager.SubscribeToMessage(TApplicationEventMessage, ApplicationEventHandler);
end;

destructor TipSystemBarsServiceAndroid.Destroy;
var
  LViewGroup: JViewGroup;
begin
  TMessageManager.DefaultManager.Unsubscribe(TApplicationEventMessage, ApplicationEventHandler);
  TMessageManager.DefaultManager.Unsubscribe(TVKStateChangeMessage, FVirtualKeyboardMessageId);
  TMessageManager.DefaultManager.Unsubscribe(TAfterCreateFormHandle, FAfterCreateFormHandleMessageId);
  TMessageManager.DefaultManager.Unsubscribe(TBeforeDestroyFormHandle, FBeforeDestroyFormHandleMessageId);
  TMessageManager.DefaultManager.Unsubscribe(TFormReleasedMessage, FFormReleasedMessageId);
  TMessageManager.DefaultManager.Unsubscribe(TFormActivateMessage, FFormActivateMessageId);
  { IFMXFullScreenWindowService }
  TPlatformServices.Current.RemovePlatformService(IFMXFullScreenWindowService);
  if Assigned(FDefaultFullScreenService) then
    TPlatformServices.Current.AddPlatformService(IFMXFullScreenWindowService, FDefaultFullScreenService);
  { IFMXWindowSystemBarsService }
  if FRegisteredBarsService then
    TPlatformServices.Current.RemovePlatformService(TipFormSystemBars.IFMXWindowSystemBarsService);
  { IFMXWindowSystemStatusBarService }
  if FRegisteredStatusBarService then
    TPlatformServices.Current.RemovePlatformService(IFMXWindowSystemStatusBarService);

  if Assigned(FOnAttachStateChangeListener) then
  begin
    LViewGroup := GetMainActivityContentView;
    if Assigned(LViewGroup) then
    begin
      if Assigned(FOnApplyWindowInsetsListener) {and (TJBuild_VERSION.JavaClass.SDK_INT >= 20)} then // Android 4.4W (Kitkat Watch / api level 20) or later
        LViewGroup.setOnApplyWindowInsetsListener(nil);
      LViewGroup.removeOnAttachStateChangeListener(FOnAttachStateChangeListener);
      if Assigned(FOnWindowFocusChangeListener) {and TOSVersion.Check(4, 3)} then // Android 4.3 (Jelly Bean / api level 18) or later
        LViewGroup.getViewTreeObserver.removeOnWindowFocusChangeListener(FOnWindowFocusChangeListener);
    end;
    FreeAndNil(FOnApplyWindowInsetsListener);
    FreeAndNil(FOnAttachStateChangeListener);
    FreeAndNil(FOnWindowFocusChangeListener);
  end;
  FreeAndNil(FOnTouchListener);
  FDefaultFormVisibility.Free;
  inherited;
end;

// StatusBar + NavigationBar + GestureBar
function TipSystemBarsServiceAndroid.DoGetAbsoluteInsets(
  const AForm: TCommonCustomForm): TRect;
var
  LView: JView;
  LWindowInsets: JWindowInsets;
  LWindowInsetsAndroid11: TAndroid11.JWindowInsets;
  LInsets: TAndroid10.JInsets;
begin
  Result := TRect.Empty;
  if TOSVersion.Check(6) then // Android 6 (Marshmallow / api level 23) or later
  begin
    LView := GetWindowDecorView;
    if Assigned(LView) then
    begin
      LWindowInsets := LView.getRootWindowInsets;
      if Assigned(LWindowInsets) then
      begin
        if TOSVersion.Check(11) then // Android 11 (api level 30) or later
        begin
          LWindowInsetsAndroid11 := TAndroid11.TJWindowInsets.Wrap(TAndroidHelper.JObjectToID(LWindowInsets));
          if Assigned(LWindowInsetsAndroid11) then
          begin
            LInsets := LWindowInsetsAndroid11.getInsets(TAndroid11.TJWindowInsets_Type.JavaClass.mandatorySystemGestures);
            if Assigned(LInsets) then
              Result := TRect.Create(LInsets.left, LInsets.top, LInsets.right, LInsets.bottom);
            LInsets := LWindowInsetsAndroid11.getInsets(TAndroid11.TJWindowInsets_Type.JavaClass.systemBars);
            if Assigned(LInsets) then
              Result := TRect.Create(Max(Result.Left, LInsets.left), Max(Result.Top, LInsets.top),
                Max(Result.Right, LInsets.right), Max(Result.Bottom, LInsets.bottom));
          end;
        end
        else
        begin
          if TOSVersion.Check(10) then // Android 10 (api level 29)
          begin
            LInsets := TAndroid10.TJWindowInsets.Wrap(TAndroidHelper.JObjectToID(LWindowInsets)).getMandatorySystemGestureInsets;
            if Assigned(LInsets) then
              Result := TRect.Create(LInsets.left, LInsets.top, LInsets.right, LInsets.bottom);
          end;
          Result := TRect.Create(Max(Result.Left, LWindowInsets.getSystemWindowInsetLeft),
            Max(Result.Top, LWindowInsets.getSystemWindowInsetTop),
            Max(Result.Right, LWindowInsets.getSystemWindowInsetRight),
            Max(Result.Bottom, LWindowInsets.getSystemWindowInsetBottom));
        end;
      end;
    end;
  end
  {$IF CompilerVersion < 34.0} // Delphi Sydney 10.4
  else if TOSVersion.Check(4, 4) then // Android 4.4 (Kitkat / api level 19) to Android 5.1 (Lollipop MR1 / api level 22)
    Result := TAndroidBeforeMarshmallow.AbsoluteSystemInsets;
  {$ENDIF}
end;

function TipSystemBarsServiceAndroid.DoGetAbsoluteTappableInsets(
  const AForm: TCommonCustomForm): TRect;
var
  LView: JView;
  LWindowInsets: JWindowInsets;
  LInsets: TAndroid10.JInsets;
begin
  Result := TRect.Empty;
  if TOSVersion.Check(10) then // Android 10 (api level 29) or later
  begin
    LView := GetWindowDecorView;
    if Assigned(LView) then
    begin
      LWindowInsets := LView.getRootWindowInsets;
      if Assigned(LWindowInsets) then
      begin
        if TOSVersion.Check(11) then // Android 11 (api level 30) or later
          LInsets := TAndroid11.TJWindowInsets.Wrap(TAndroidHelper.JObjectToID(LWindowInsets)).getInsets(
            TAndroid11.TJWindowInsets_Type.JavaClass.tappableElement)
        else
          LInsets := TAndroid10.TJWindowInsets.Wrap(TAndroidHelper.JObjectToID(LWindowInsets)).getTappableElementInsets;
        if Assigned(LInsets) then
          Result := TRect.Create(LInsets.left, LInsets.top, LInsets.right, LInsets.bottom);
      end;
    end;
  end
  else
    Result := DoGetAbsoluteInsets(AForm);
end;

procedure TipSystemBarsServiceAndroid.DoSetVisibility(
  const AForm: TCommonCustomForm;
  const AMode: TipFormSystemBars.TVisibilityMode);
var
  LActivity: JActivity;
  LWindow: JWindow;
  LWindowAndroid11: TAndroid11.JWindow;
  LView: JFormView;
  LMainActivityContentView: JViewGroup;
  LViewAndroid11: TAndroid11.JView;
  LWindowInsetsController: TAndroid11.JWindowInsetsController;
  LSystemUiVisibility: Integer;
  LSystemUiVisibilityMask: Integer;
  LWinParams: JWindowManager_LayoutParams;
  LWinParamsAndroid9: TAndroid9.JWindowManager_LayoutParams;
  LHasGestureNavigationBar: Boolean;
  LRect: TRect;
begin
  {$IF CompilerVersion < 34.0} // Delphi Sydney 10.4
  if not TOSVersion.Check(4, 4) then // Supported only Android 4.4 (Kitkat / api level 19) or later
    Exit;
  {$ENDIF}
  if not CanFormChangeSystemBars(AForm) then
    Exit;

  LWindow := nil;
  LActivity := TAndroidHelper.Activity;
  if Assigned(LActivity) then
    LWindow := LActivity.getWindow;
  if AForm.IsHandleAllocated then
    LView := TAndroidWindowHandle(AForm.Handle).View
  else
    LView := nil;
  LHasGestureNavigationBar := HasGestureNavigationBar(AForm);

  SetStatusBarBackgroundColor(AForm, AForm.SystemStatusBar.BackgroundColor);
  SetNavigationBarBackgroundColor(AForm, AForm.SystemBars.NavigationBarBackgroundColor);
  if TOSVersion.Check(11) then // Android 11 (api level 30) or later
  begin
    if Assigned(LWindow) then
    begin
      LWindowAndroid11 := TAndroid11.TJWindowEx.Wrap((LWindow as ILocalObject).GetObjectID);
      if Assigned(LWindowAndroid11) then
        LWindowAndroid11.setDecorFitsSystemWindows((AMode = TipFormSystemBars.TVisibilityMode.Visible) and not LHasGestureNavigationBar);
      LWinParams := LWindow.getAttributes;
      if Assigned(LWinParams) then
      begin
        LWinParamsAndroid9 := TAndroid9.TJWindowManager_LayoutParams.Wrap(TAndroidHelper.JObjectToID(LWinParams));
        if Assigned(LWinParamsAndroid9) then
        begin
          if AMode = TipFormSystemBars.TVisibilityMode.Invisible then
            LWinParamsAndroid9.layoutInDisplayCutoutMode := TAndroid9.TJWindowManager_LayoutParams.JavaClass.LAYOUT_IN_DISPLAY_CUTOUT_MODE_SHORT_EDGES
          else
            LWinParamsAndroid9.layoutInDisplayCutoutMode := TAndroid9.TJWindowManager_LayoutParams.JavaClass.LAYOUT_IN_DISPLAY_CUTOUT_MODE_DEFAULT;
        end;
        LWindow.setAttributes(LWinParams);
      end;
    end;

    if Assigned(LView) then
    begin
      LViewAndroid11 := TAndroid11.TJView.Wrap(TAndroidHelper.JObjectToID(LView));
      if Assigned(LViewAndroid11) then
      begin
        LWindowInsetsController := LViewAndroid11.getWindowInsetsController;
        if Assigned(LWindowInsetsController) then
        begin
          case AMode of
            TipFormSystemBars.TVisibilityMode.Visible,
            TipFormSystemBars.TVisibilityMode.VisibleAndOverlap:
              begin
                LWindowInsetsController.show(TAndroid11.TJWindowInsets_Type.JavaClass.statusBars or
                  TAndroid11.TJWindowInsets_Type.JavaClass.navigationBars);
              end;
            TipFormSystemBars.TVisibilityMode.Invisible:
              begin
                LWindowInsetsController.setSystemBarsBehavior(TAndroid11.TJWindowInsetsController.JavaClass.BEHAVIOR_SHOW_TRANSIENT_BARS_BY_SWIPE);
                LWindowInsetsController.hide(TAndroid11.TJWindowInsets_Type.JavaClass.statusBars or
                  TAndroid11.TJWindowInsets_Type.JavaClass.navigationBars);
              end;
          end;
        end;
      end;
    end;
    SetStatusBarBackgroundColor(AForm, AForm.SystemStatusBar.BackgroundColor);
    SetNavigationBarBackgroundColor(AForm, AForm.SystemBars.NavigationBarBackgroundColor);
  end
  else // BEFORE Android 11 (api level 30)
  begin
    LSystemUiVisibilityMask := 0;
    if Assigned(LWindow) then
      LWinParams := LWindow.getAttributes
    else
      LWinParams := nil;

    {$IF CompilerVersion < 34.0} // Delphi Sydney 10.4
    if TOSVersion.Check(4, 4) and not TOSVersion.Check(5) and // Android 4.4 (Kitkat / api level 19) and Android 4.4W (Kitkat Watch / api level 20)
      Assigned(LWinParams) then
    begin
      case AMode of
        TipFormSystemBars.TVisibilityMode.Visible:
          begin
            LWinParams.flags := LWinParams.flags and not
              (TJWindowManager_LayoutParams.JavaClass.FLAG_TRANSLUCENT_STATUS or
              TJWindowManager_LayoutParams.JavaClass.FLAG_TRANSLUCENT_NAVIGATION);
          end;
        TipFormSystemBars.TVisibilityMode.VisibleAndOverlap:
          begin
            LWinParams.flags := LWinParams.flags or
              TJWindowManager_LayoutParams.JavaClass.FLAG_TRANSLUCENT_STATUS or
              TJWindowManager_LayoutParams.JavaClass.FLAG_TRANSLUCENT_NAVIGATION;
          end;
        TipFormSystemBars.TVisibilityMode.Invisible: ;
      end;
    end;
    {$ENDIF}
    if Assigned(LWinParams)
      {$IF CompilerVersion < 34.0} // Delphi Sydney 10.4
      and TOSVersion.Check(5)
      {$ENDIF} then // Android 5.0 (Lollipop / api level 21) or later
    begin
      LWinParams.flags := LWinParams.flags and not
        (TJWindowManager_LayoutParams.JavaClass.FLAG_TRANSLUCENT_STATUS or
         TJWindowManager_LayoutParams.JavaClass.FLAG_TRANSLUCENT_NAVIGATION);
    end;
    if Assigned(LWinParams) then
    begin
      if TOSVersion.Check(9) then // Android 9 (Pie / api level 28) or later
      begin
        LWinParamsAndroid9 := TAndroid9.TJWindowManager_LayoutParams.Wrap(TAndroidHelper.JObjectToID(LWinParams));
        if Assigned(LWinParamsAndroid9) then
        begin
          if AMode = TipFormSystemBars.TVisibilityMode.Invisible then
            LWinParamsAndroid9.layoutInDisplayCutoutMode := TAndroid9.TJWindowManager_LayoutParams.JavaClass.LAYOUT_IN_DISPLAY_CUTOUT_MODE_SHORT_EDGES
          else
            LWinParamsAndroid9.layoutInDisplayCutoutMode := TAndroid9.TJWindowManager_LayoutParams.JavaClass.LAYOUT_IN_DISPLAY_CUTOUT_MODE_DEFAULT;
        end;
      end;
      if AMode = TipFormSystemBars.TVisibilityMode.Invisible then
        LWinParams.flags := (LWinParams.flags or TJWindowManager_LayoutParams.JavaClass.FLAG_FULLSCREEN)
          and not TJWindowManager_LayoutParams.JavaClass.FLAG_FORCE_NOT_FULLSCREEN
      else
        LWinParams.flags := (LWinParams.flags or TJWindowManager_LayoutParams.JavaClass.FLAG_FORCE_NOT_FULLSCREEN)
          and not TJWindowManager_LayoutParams.JavaClass.FLAG_FULLSCREEN;
      LWindow.setAttributes(LWinParams);
    end;

    if TOSVersion.Check(6) then // Android 6 (Marshmallow / api level 23) or later
      LSystemUiVisibilityMask := LSystemUiVisibilityMask or TJView.JavaClass.SYSTEM_UI_FLAG_LIGHT_STATUS_BAR;
    if TOSVersion.Check(8) then // Android 8.0 (Oreo / api level 26) or later
      LSystemUiVisibilityMask := LSystemUiVisibilityMask or TJView.JavaClass.SYSTEM_UI_FLAG_LIGHT_NAVIGATION_BAR;
    if Assigned(LView)
      {$IF CompilerVersion < 34.0} // Delphi Sydney 10.4
      and TOSVersion.Check(4, 4)
      {$ENDIF} then // Android 4.4 (Kitkat / api level 19) or later
    begin
      case AMode of
        TipFormSystemBars.TVisibilityMode.Visible:
          begin
            if LHasGestureNavigationBar then
            begin
              LSystemUiVisibility := TJView.JavaClass.SYSTEM_UI_FLAG_VISIBLE or
                TJView.JavaClass.SYSTEM_UI_FLAG_LAYOUT_STABLE or
                TJView.JavaClass.SYSTEM_UI_FLAG_LAYOUT_FULLSCREEN or
                TJView.JavaClass.SYSTEM_UI_FLAG_LAYOUT_HIDE_NAVIGATION;
            end
            else
              LSystemUiVisibility := TJView.JavaClass.SYSTEM_UI_FLAG_VISIBLE;
          end;
        TipFormSystemBars.TVisibilityMode.VisibleAndOverlap:
          begin
            LSystemUiVisibility := TJView.JavaClass.SYSTEM_UI_FLAG_VISIBLE or
              TJView.JavaClass.SYSTEM_UI_FLAG_LAYOUT_STABLE or
              TJView.JavaClass.SYSTEM_UI_FLAG_LAYOUT_FULLSCREEN or
              TJView.JavaClass.SYSTEM_UI_FLAG_LAYOUT_HIDE_NAVIGATION;
          end;
        TipFormSystemBars.TVisibilityMode.Invisible:
          begin
            LSystemUiVisibility := TJView.JavaClass.SYSTEM_UI_FLAG_LAYOUT_STABLE or
              TJView.JavaClass.SYSTEM_UI_FLAG_LAYOUT_HIDE_NAVIGATION or
              TJView.JavaClass.SYSTEM_UI_FLAG_LAYOUT_FULLSCREEN or
              TJView.JavaClass.SYSTEM_UI_FLAG_HIDE_NAVIGATION or // hide nav bar
              TJView.JavaClass.SYSTEM_UI_FLAG_FULLSCREEN or // hide status bar
              TJView.JavaClass.SYSTEM_UI_FLAG_IMMERSIVE_STICKY;
          end;
      else
        LSystemUiVisibility := 0;
      end;
      LView.setSystemUiVisibility((LView.getSystemUiVisibility and LSystemUiVisibilityMask) or LSystemUiVisibility);
    end;
  end;

  if Assigned(LView) then
  begin
    LView.setFitsSystemWindows((AMode = TipFormSystemBars.TVisibilityMode.Visible) and not LHasGestureNavigationBar);
    RefreshView(LView);

    LMainActivityContentView := GetMainActivityContentView;
    if Assigned(LMainActivityContentView) then
    begin
      if HasGestureNavigationBar(AForm) and (AMode = TipFormSystemBars.TVisibilityMode.Visible) then
      begin
        LRect := DoGetAbsoluteTappableInsets(AForm);
        LMainActivityContentView.setPadding(LRect.Left, LRect.Top, LRect.Right, LRect.Bottom);
      end
      else
        LMainActivityContentView.setPadding(0, 0, 0, 0);
    end;
  end;

  if AMode <> TipFormSystemBars.TVisibilityMode.Invisible then
    FDefaultFormVisibility.AddOrSetValue(AForm, AMode);
  // Set again when the HasGestureNavigationBar changes due the config
  if LHasGestureNavigationBar <> HasGestureNavigationBar(AForm) then
    DoSetVisibility(AForm, AMode);
end;

procedure TipSystemBarsServiceAndroid.FormActivate(const ASender: TObject;
  const AMessage: TMessage);
var
  LForm: TCommonCustomForm;
begin
  if ASender is TCommonCustomForm then
  begin
    LForm := TCommonCustomForm(ASender);
    SetVisibility(LForm, LForm.SystemStatusBar.Visibility);
  end;
end;

procedure TipSystemBarsServiceAndroid.FormReleased(const ASender: TObject;
  const AMessage: TMessage);
begin
  if ASender is TCommonCustomForm then
    FDefaultFormVisibility.Remove(TCommonCustomForm(ASender));
end;

function TipSystemBarsServiceAndroid.GetAbsoluteInsets(
  const AForm: TCommonCustomForm): TRect;
var
  LAbsoluteTappableInsets: TRect;
begin
  Result := TRect.Empty;
  if not HasSystemBars(AForm) then
    Exit;
  case AForm.SystemBars.Visibility of
    TipFormSystemBars.TVisibilityMode.Visible: // GestureBar
      begin
        Result := DoGetAbsoluteInsets(AForm);
        LAbsoluteTappableInsets := DoGetAbsoluteTappableInsets(AForm);
        Result := TRect.Create(Max(Result.Left - LAbsoluteTappableInsets.Left, 0),
          Max(Result.Top - LAbsoluteTappableInsets.Top, 0),
          Max(Result.Right - LAbsoluteTappableInsets.Right, 0),
          Max(Result.Bottom - LAbsoluteTappableInsets.Bottom, 0));
      end;
    TipFormSystemBars.TVisibilityMode.Invisible:; // None
    TipFormSystemBars.TVisibilityMode.VisibleAndOverlap: Result := DoGetAbsoluteInsets(AForm); // StatusBar + NavigationBar + GestureBar
  end;
end;

function TipSystemBarsServiceAndroid.GetAbsoluteTappableInsets(
  const AForm: TCommonCustomForm): TRect;
begin
  Result := TRect.Empty;
  if not HasSystemBars(AForm) then
    Exit;
  case AForm.SystemBars.Visibility of
    TipFormSystemBars.TVisibilityMode.Visible:; // None
    TipFormSystemBars.TVisibilityMode.Invisible:; // None
    TipFormSystemBars.TVisibilityMode.VisibleAndOverlap: Result := DoGetAbsoluteTappableInsets(AForm); // StatusBar + NavigationBar
  end;
end;

function TipSystemBarsServiceAndroid.GetFullScreen(
  const AForm: TCommonCustomForm): Boolean;
var
  LSystemBars: TipFormSystemBars;
begin
  Result := False;
  if Assigned(AForm) then
  begin
    LSystemBars := AForm.SystemBars;
    if Assigned(LSystemBars) then
      Result := LSystemBars.Visibility = TipFormSystemBars.TVisibilityMode.Invisible;
  end;
end;

function TipSystemBarsServiceAndroid.GetInsets(const AForm: TCommonCustomForm): TRectF;

  function GetCurrentInsets(const AForm: TCommonCustomForm): TRectF;
  var
    LFormSystemBars: TipFormSystemBars;
  begin
    LFormSystemBars := AForm.SystemBars;
    if Assigned(LFormSystemBars) then
      Result := LFormSystemBars.Insets
    else
      Result := TRectF.Empty;
  end;

begin
  if not Assigned(AForm) then
    Result := TRectF.Empty
  else if (not AForm.IsHandleAllocated) or (not AForm.Active) then
    Result := GetCurrentInsets(AForm)
  else
  begin
    Result := AbsoluteRectToScreenScaled(GetAbsoluteInsets(AForm));
    Result := RemoveKeyboardOverlappedBars(Result, AForm);
  end;
end;

class function TipSystemBarsServiceAndroid.GetMainActivityContentView: JViewGroup;
var
  LMainActivity: JFMXNativeActivity;
begin
  LMainActivity := MainActivity;
  if LMainActivity <> nil then
  begin
    {$IF CompilerVersion >= 34.0} // Delphi Sydney 10.4
    Result := LMainActivity.getContentView;
    {$ELSE}
    Result := LMainActivity.getViewGroup;
    {$ENDIF}
  end;
end;

function TipSystemBarsServiceAndroid.GetTappableInsets(const AForm: TCommonCustomForm): TRectF;

  function GetCurrentTappableInsets(const AForm: TCommonCustomForm): TRectF;
  var
    LFormSystemBars: TipFormSystemBars;
  begin
    LFormSystemBars := AForm.SystemBars;
    if Assigned(LFormSystemBars) then
      Result := LFormSystemBars.TappableInsets
    else
      Result := TRectF.Empty;
  end;

begin
  if not Assigned(AForm) then
    Result := TRectF.Empty
  else if (not AForm.IsHandleAllocated) or (not AForm.Active) then
    Result := GetCurrentTappableInsets(AForm)
  else
  begin
    Result := AbsoluteRectToScreenScaled(GetAbsoluteTappableInsets(AForm));
    Result := RemoveKeyboardOverlappedBars(Result, AForm);
  end;
end;

class function TipSystemBarsServiceAndroid.GetWindowDecorView: JView;
var
  LActivity: JActivity;
  LWindow: JWindow;
begin
  Result := nil;
  LActivity := TAndroidHelper.Activity;
  if Assigned(LActivity) then
  begin
    LWindow := LActivity.getWindow;
    if Assigned(LWindow) then
      Result := LWindow.getDecorView;
  end;
end;

function TipSystemBarsServiceAndroid.HasGestureNavigationBar(
  const AForm: TCommonCustomForm): Boolean;
begin
  if TOSVersion.Check(10) then // Android 10 (api level 29) or later
    Result := HasSystemBars(AForm) and AForm.Active and (DoGetAbsoluteInsets(AForm) <> DoGetAbsoluteTappableInsets(AForm))
  else
    Result := False;
end;

function TipSystemBarsServiceAndroid.HasSystemBars(
  const AForm: TCommonCustomForm): Boolean;
var
  LFormSystemBars: TipFormSystemBars;
begin
  Result := False;
  if Assigned(AForm) and AForm.IsHandleAllocated then
  begin
    LFormSystemBars := AForm.SystemBars;
    if Assigned(LFormSystemBars) then
      Result := LFormSystemBars.Visibility <> TipFormSystemBars.TVisibilityMode.Invisible;
  end;
end;

function TipSystemBarsServiceAndroid.IsDarkTheme: Boolean;
{$IF CompilerVersion >= 34.0} // Delphi Sydney 10.4
var
  LSystemAppearanceService: IFMXSystemAppearanceService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXSystemAppearanceService, LSystemAppearanceService) then
    Result := LSystemAppearanceService.ThemeKind = TSystemThemeKind.Dark
  else
    Result := False;
{$ELSE}
begin
  Result := False;
{$ENDIF}
end;

procedure TipSystemBarsServiceAndroid.RefreshView(const AView: JView);
var
  LViewParent: JViewParent;
begin
  if Assigned(AView) then
  begin
    LViewParent := AView.getParent;
    if Assigned(LViewParent) then
    begin
      LViewParent.requestFitSystemWindows;
      LViewParent.requestLayout;
    end;
  end;
end;

function TipSystemBarsServiceAndroid.RemoveKeyboardOverlappedBars(
  const AInsets: TRectF; const AForm: TCommonCustomForm): TRectF;

  // This is really necessary because the TVKStateChangeMessage in android is not accurate, sometimes there are false positives.
  // The problem is not from embarcadero code, this is a android problem (challenge) that was solve in api 30
  function IsIMEReallyVisible: Boolean;
  var
    LView: JView;
    LWindowInsets: JWindowInsets;
    LWindowInsetsAndroid11: TAndroid11.JWindowInsets;
  begin
    Result := True;
    if TOSVersion.Check(11) then // Android 11 (api level 30) or later
    begin
      LView := GetWindowDecorView;
      if Assigned(LView) then
      begin
        LWindowInsets := LView.getRootWindowInsets;
        if Assigned(LWindowInsets) then
        begin
          LWindowInsetsAndroid11 := TAndroid11.TJWindowInsets.Wrap(TAndroidHelper.JObjectToID(LWindowInsets));
          if Assigned(LWindowInsetsAndroid11) then
            Result := LWindowInsetsAndroid11.isVisible(TAndroid11.TJWindowInsets_Type.JavaClass.ime);
        end;
      end;
    end;
  end;

begin
  Result := AInsets;
  if (not FVirtualKeyboardBounds.IsEmpty) and (AInsets <> TRectF.Empty) and Assigned(AForm) and AForm.Active then
    if (FVirtualKeyboardBounds.Top <> 0) and (FVirtualKeyboardBounds.Left = 0) and // Check if virtual keyboard is in bottom
      (Result.Bottom <> Max(Result.Bottom - FVirtualKeyboardBounds.Height, 0)) and
      IsIMEReallyVisible then
    begin
      Result.Bottom := Max(Result.Bottom - FVirtualKeyboardBounds.Height, 0); // Removing bottom system bars
    end;
end;

procedure TipSystemBarsServiceAndroid.SetFullScreen(
  const AForm: TCommonCustomForm; const AValue: Boolean);
var
  LDefaultFormVisibility: TipFormSystemBars.TVisibilityMode;
begin
  if Assigned(AForm) then
  begin
    if AValue then
      AForm.SystemBars.Visibility := TipFormSystemBars.TVisibilityMode.Invisible
    else if GetFullScreen(AForm) then
    begin
      if FDefaultFormVisibility.TryGetValue(AForm, LDefaultFormVisibility) then
        AForm.SystemBars.Visibility := LDefaultFormVisibility
      else
        AForm.SystemBars.Visibility := TipFormSystemBars.DefaultVisibility;
    end;
  end;
end;

procedure TipSystemBarsServiceAndroid.SetNavigationBarBackgroundColor(
  const AForm: TCommonCustomForm; const AColor: TAlphaColor);
var
  LNavigationBarLight: Boolean;
  LNavigationBarJColor: Integer;
  LActivity: JActivity;
  LWindow: JWindow;
  LWindowAndroid10: TAndroid10.JWindow;
  LView: JFormView;
  LViewAndroid11: TAndroid11.JView;
  LWindowInsetsController: TAndroid11.JWindowInsetsController;
  LSystemUiVisibility: Integer;
  LNewAlphaColor: TAlphaColor;
begin
  {$IF CompilerVersion < 34.0} // Delphi Sydney 10.4
  if not TOSVersion.Check(5) then // Supported only Android 5.0 (Lollipop / api level 21) or later
    Exit;
  {$ENDIF}
  if not CanFormChangeSystemBars(AForm) then
    Exit;

  LNewAlphaColor := AColor;
  case AForm.SystemBars.Visibility of
    TipFormSystemBars.TVisibilityMode.Visible:
      begin
        if AColor = TAlphaColors.Null then // Like iOS
        begin
          // BEFORE the Android 8.0 (api level 26), the navigation bar buttons is always
          // white, then is better set the background color to black as default
          if IsDarkTheme or not TOSVersion.Check(8) then
            LNewAlphaColor := TAlphaColors.Black
          else
            LNewAlphaColor := TAlphaColors.White;
        end;
        if HasGestureNavigationBar(AForm) then
          TAlphaColorRec(LNewAlphaColor).A := 0;
      end;
    TipFormSystemBars.TVisibilityMode.VisibleAndOverlap:
      if HasGestureNavigationBar(AForm) then
        TAlphaColorRec(LNewAlphaColor).A := 0;
    TipFormSystemBars.TVisibilityMode.Invisible: TAlphaColorRec(LNewAlphaColor).A := 0;
  end;

  LNavigationBarLight := Luminance(LNewAlphaColor) > 0.5;
  LNavigationBarJColor := TAndroidHelper.AlphaColorToJColor(LNewAlphaColor);
  LActivity := TAndroidHelper.Activity;
  if Assigned(LActivity) then
    LWindow := LActivity.getWindow
  else
    LWindow := nil;
  if AForm.IsHandleAllocated then
    LView := TAndroidWindowHandle(AForm.Handle).View
  else
    LView := nil;

  if TOSVersion.Check(11) then // Android 11 (api level 30) or later
  begin
    if Assigned(LWindow) then
    begin
      LWindowAndroid10 := TAndroid10.TJWindow.Wrap(TAndroidHelper.JObjectToID(LWindow));
      if Assigned(LWindowAndroid10) then
        LWindowAndroid10.setNavigationBarContrastEnforced(False);
      if LWindow.getNavigationBarColor <> LNavigationBarJColor then
      begin
        LWindow.setNavigationBarColor(LNavigationBarJColor);
        RefreshView(LView);
      end;
    end;
    if Assigned(LView) then
    begin
      LViewAndroid11 := TAndroid11.TJView.Wrap(TAndroidHelper.JObjectToID(LView));
      if Assigned(LViewAndroid11) then
      begin
        LWindowInsetsController := LViewAndroid11.getWindowInsetsController;
        if Assigned(LWindowInsetsController) then
        begin
          if LNavigationBarLight then
            LWindowInsetsController.setSystemBarsAppearance(TAndroid11.TJWindowInsetsController.JavaClass.APPEARANCE_LIGHT_NAVIGATION_BARS,
              TAndroid11.TJWindowInsetsController.JavaClass.APPEARANCE_LIGHT_NAVIGATION_BARS)
          else
            LWindowInsetsController.setSystemBarsAppearance(0, TAndroid11.TJWindowInsetsController.JavaClass.APPEARANCE_LIGHT_NAVIGATION_BARS);
        end;
      end;
    end;
  end
  else // BEFORE Android 11 (api level 30)
  begin
    if TOSVersion.Check(8) and Assigned(LView) then // Android 8.0 (Oreo / api level 26) or later
    begin
      if LNavigationBarLight then
        LSystemUiVisibility := LView.getSystemUiVisibility or TJView.JavaClass.SYSTEM_UI_FLAG_LIGHT_NAVIGATION_BAR
      else
        LSystemUiVisibility := LView.getSystemUiVisibility and not TJView.JavaClass.SYSTEM_UI_FLAG_LIGHT_NAVIGATION_BAR;
      LView.setSystemUiVisibility(LSystemUiVisibility);
    end;
    if TOSVersion.Check(10) and Assigned(LWindow) then // Android 10 (api level 29) or later
    begin
      LWindowAndroid10 := TAndroid10.TJWindow.Wrap(TAndroidHelper.JObjectToID(LWindow));
      if Assigned(LWindowAndroid10) then
        LWindowAndroid10.setNavigationBarContrastEnforced(False);
    end;
    if Assigned(LWindow)
      {$IF CompilerVersion < 34.0} // Delphi Sydney 10.4
      and TOSVersion.Check(5)
      {$ENDIF} then // Android 5.0 (Lollipop / api level 21) or later
    begin
      if LWindow.getNavigationBarColor <> LNavigationBarJColor then
      begin
        LWindow.setNavigationBarColor(LNavigationBarJColor);
        RefreshView(LView);
      end;
    end;
  end;
end;

procedure TipSystemBarsServiceAndroid.SetShowFullScreenIcon(
  const AForm: TCommonCustomForm; const AValue: Boolean);
begin
end;

procedure TipSystemBarsServiceAndroid.SetStatusBarBackgroundColor(
  const AForm: TCommonCustomForm; const AColor: TAlphaColor);
var
  LStatusBarLight: Boolean;
  LStatusBarJColor: Integer;
  LActivity: JActivity;
  LWindow: JWindow;
  LWindowAndroid10: TAndroid10.JWindow;
  LView: JFormView;
  LViewAndroid11: TAndroid11.JView;
  LWindowInsetsController: TAndroid11.JWindowInsetsController;
  LSystemUiVisibility: Integer;
  LNewAlphaColor: TAlphaColor;
  LMainActivityContentView: JViewGroup;
begin
  {$IF CompilerVersion < 34.0} // Delphi Sydney 10.4
  if not TOSVersion.Check(5) then // Supported only Android 5.0 (Lollipop / api level 21) or later
    Exit;
  {$ENDIF}
  if not CanFormChangeSystemBars(AForm) then
    Exit;

  LNewAlphaColor := AColor;
  case AForm.SystemBars.Visibility of
    TipFormSystemBars.TVisibilityMode.Visible:
      if AColor = TAlphaColors.Null then // Like iOS
      begin
        if IsDarkTheme then
          LNewAlphaColor := TAlphaColors.Black
        else
          LNewAlphaColor := TAlphaColors.White;
      end;
    TipFormSystemBars.TVisibilityMode.VisibleAndOverlap:;
    TipFormSystemBars.TVisibilityMode.Invisible: TAlphaColorRec(LNewAlphaColor).A := 0;
  end;

  LStatusBarLight := Luminance(LNewAlphaColor) > 0.5;
  LStatusBarJColor := TAndroidHelper.AlphaColorToJColor(LNewAlphaColor);
  LActivity := TAndroidHelper.Activity;
  if Assigned(LActivity) then
    LWindow := LActivity.getWindow
  else
    LWindow := nil;
  if AForm.IsHandleAllocated then
    LView := TAndroidWindowHandle(AForm.Handle).View
  else
    LView := nil;

  // Setting the configurations
  if TOSVersion.Check(11) then // Android 11 (api level 30) or later
  begin
    if Assigned(LWindow) then
    begin
      LWindowAndroid10 := TAndroid10.TJWindow.Wrap(TAndroidHelper.JObjectToID(LWindow));
      if Assigned(LWindowAndroid10) then
        LWindowAndroid10.setStatusBarContrastEnforced(False);
      if LWindow.getStatusBarColor <> LStatusBarJColor then
      begin
        LWindow.setStatusBarColor(LStatusBarJColor);
        RefreshView(LView);
      end;
    end;
    if Assigned(LView) then
    begin
      if TOSVersion.Check(12) then
      begin
        LViewAndroid11 := TAndroid11.TJView.Wrap(TAndroidHelper.JObjectToID(LView));
        if Assigned(LViewAndroid11) then
        begin
          LWindowInsetsController := LViewAndroid11.getWindowInsetsController;
          if Assigned(LWindowInsetsController) then
          begin
            if LStatusBarLight then
              LWindowInsetsController.setSystemBarsAppearance(TAndroid11.TJWindowInsetsController.JavaClass.APPEARANCE_LIGHT_STATUS_BARS,
                TAndroid11.TJWindowInsetsController.JavaClass.APPEARANCE_LIGHT_STATUS_BARS)
            else
              LWindowInsetsController.setSystemBarsAppearance(0, TAndroid11.TJWindowInsetsController.JavaClass.APPEARANCE_LIGHT_STATUS_BARS);
          end;
        end;
      end
      // In some versions of Android 11 there is a known issue where the WindowInsetsController.setSystemBarsAppearance
      // doesn't work when the app's theme declares <item name="android:windowLightStatusBar">true/false</item>, directly
      // or indirectly in styles.xml, which this is the case with the FMX. The best workaround to this is using the old
      // method (even being obsolete).
      else
      begin
        if LStatusBarLight then
          LSystemUiVisibility := LView.getSystemUiVisibility or TJView.JavaClass.SYSTEM_UI_FLAG_LIGHT_STATUS_BAR
        else
          LSystemUiVisibility := LView.getSystemUiVisibility and not TJView.JavaClass.SYSTEM_UI_FLAG_LIGHT_STATUS_BAR;
        LView.setSystemUiVisibility(LSystemUiVisibility);
      end;
    end;
  end
  else // BEFORE Android 11 (api level 30)
  begin
    if TOSVersion.Check(6) and Assigned(LView) then // Android 6 (Marshmallow / api level 23) or later
    begin
      if LStatusBarLight then
        LSystemUiVisibility := LView.getSystemUiVisibility or TJView.JavaClass.SYSTEM_UI_FLAG_LIGHT_STATUS_BAR
      else
        LSystemUiVisibility := LView.getSystemUiVisibility and not TJView.JavaClass.SYSTEM_UI_FLAG_LIGHT_STATUS_BAR;
      LView.setSystemUiVisibility(LSystemUiVisibility);
    end;
    if TOSVersion.Check(10) and Assigned(LWindow) then // Android 10 (api level 29) or later
    begin
      LWindowAndroid10 := TAndroid10.TJWindow.Wrap(TAndroidHelper.JObjectToID(LWindow));
      if Assigned(LWindowAndroid10) then
        LWindowAndroid10.setStatusBarContrastEnforced(False);
    end;
    if Assigned(LWindow)
      {$IF CompilerVersion < 34.0} // Delphi Sydney 10.4
      and TOSVersion.Check(5)
      {$ENDIF} then // Android 5.0 (Lollipop / api level 21) or later
    begin
      if LWindow.getStatusBarColor <> LStatusBarJColor then
      begin
        LWindow.setStatusBarColor(LStatusBarJColor);
        RefreshView(LView);
      end;
    end;
  end;

  if AForm.Active and (AForm.SystemBars.Visibility = TipFormSystemBars.TVisibilityMode.Visible) and HasGestureNavigationBar(AForm) then
  begin
    LMainActivityContentView := GetMainActivityContentView;
    if Assigned(LMainActivityContentView) then
      LMainActivityContentView.setBackgroundColor(TAndroidHelper.AlphaColorToJColor(TAlphaColors.White));
  end;
end;

procedure TipSystemBarsServiceAndroid.SetVisibility(
  const AForm: TCommonCustomForm;
  const AMode: TipFormSystemBars.TVisibilityMode);
begin
  FOnApplyWindowInsetsListener.ChangeChecksEnabled := False;
  try
    DoSetVisibility(AForm, AMode);
  finally
    FOnApplyWindowInsetsListener.ChangeChecksEnabled := True;
  end;
  CheckInsetsChanges(AForm);
end;

// When in invisible mode, the status bar will be showed when the application becames active (after inactive) or when one message dialogs
// close or when virtual keyboard close. I believe that this is generated by the Embarcadero's FullScreenManager.java listener when detecting
// changes in the state of the application. As we cannot disable it, then we will detect when this occurs in order to be able to return to
// the default Invisible state.
procedure TipSystemBarsServiceAndroid.TryFixInvisibleMode;
var
  LForm: TCommonCustomForm;
  LFormSystemBars: TipFormSystemBars;
begin
  if Assigned(Screen) then
  begin
    LForm := Screen.ActiveForm;
    if Assigned(LForm) then
    begin
      LFormSystemBars := LForm.SystemBars;
      if Assigned(LFormSystemBars) and (LFormSystemBars.Visibility = TipFormSystemBars.TVisibilityMode.Invisible) then
        FSystemBarsServiceAndroid.SetVisibility(LForm, LFormSystemBars.Visibility);
    end;
  end;
end;

procedure TipSystemBarsServiceAndroid.VirtualKeyboardChangeHandler(
  const ASender: TObject; const AMessage: System.Messaging.TMessage);
begin
  if AMessage is TVKStateChangeMessage then
  begin
    if TVKStateChangeMessage(AMessage).KeyboardVisible then
    begin
      FVirtualKeyboardBounds := TVKStateChangeMessage(AMessage).KeyboardBounds;
      if FVirtualKeyboardBounds.IsEmpty then
        FVirtualKeyboardBounds := TRect.Empty;
    end
    else
      FVirtualKeyboardBounds := TRect.Empty;
    if Assigned(Screen) then
      CheckInsetsChanges(Screen.ActiveForm);
    if FVirtualKeyboardBounds = TRect.Empty then
      TryFixInvisibleMode;
  end;
end;

initialization
  TRegTypes.RegisterType('iPub.FMX.SystemBars.Android.TAndroid10.JInsets', TypeInfo(iPub.FMX.SystemBars.Android.TAndroid10.JInsets));
  TRegTypes.RegisterType('iPub.FMX.SystemBars.Android.TAndroid11.JWindowInsetsController', TypeInfo(iPub.FMX.SystemBars.Android.TAndroid11.JWindowInsetsController));
  FSystemBarsServiceAndroid := TipSystemBarsServiceAndroid.Create;
{$ELSE}
implementation
{$ENDIF}
end.
