# Delphi iOS - Handle incoming url
This tutorial will explain how to find out which url opened your Delphi Firemonkey iOS app. This work for Custom Schemes and for Universal Links. Let's go.
In the file **iOSapi.Foundation.pas** put the code:

      // Dave No﻿ttage code (https://www.delphiworlds.com/)
      NSUserActivityPersistentIdentifier = NSString;
    
      TNSUserActivityBlockMethod1 = procedure(inputStream: NSInputStream; outputStream: NSOutputStream; error: NSError) of object;
      TNSUserActivityBlockMethod2 = procedure of object;
    
      NSUserActivityClass = interface(NSObjectClass)
        ['{412EAEBF-5927-4D01-B83F-69D3B5DFE7B5}']
        {class} procedure deleteAllSavedUserActivitiesWithCompletionHandler(handler: TNSUserActivityBlockMethod2); cdecl;
        [MethodName('deleteSavedUserActivitiesWithPersistentIdentifiers:completionHandler:')]
        {class} procedure deleteSavedUserActivitiesWithPersistentIdentifiers(persistentIdentifiers: NSArray; handler: TNSUserActivityBlockMethod2); cdecl;
      end;
    
      NSUserActivity = interface(NSObject)
        ['{B8C2F6C9-31FE-4282-B7CA-98C96E163033}']
        function activityType: NSString; cdecl;
        procedure addUserInfoEntriesFromDictionary(otherDictionary: NSDictionary); cdecl;
        procedure becomeCurrent; cdecl;
        function delegate: Pointer; cdecl;
        function expirationDate: NSDate; cdecl;
        procedure getContinuationStreamsWithCompletionHandler(completionHandler: TNSUserActivityBlockMethod1); cdecl;
        function initWithActivityType(activityType: NSString): Pointer; cdecl;
        procedure invalidate; cdecl;
        function isEligibleForHandoff: Boolean; cdecl;
        function isEligibleForPrediction: Boolean; cdecl;
        function isEligibleForPublicIndexing: Boolean; cdecl;
        function isEligibleForSearch: Boolean; cdecl;
        function keywords: NSSet; cdecl;
        function needsSave: Boolean; cdecl;
        function persistentIdentifier: NSUserActivityPersistentIdentifier; cdecl;
        function referrerURL: NSURL; cdecl;
        function requiredUserInfoKeys: NSSet; cdecl;
        procedure resignCurrent; cdecl;
        procedure setDelegate(delegate: Pointer); cdecl;
        procedure setEligibleForHandoff(eligibleForHandoff: Boolean); cdecl;
        procedure setEligibleForPrediction(eligibleForPrediction: Boolean); cdecl;
        procedure setEligibleForPublicIndexing(eligibleForPublicIndexing: Boolean); cdecl;
        procedure setEligibleForSearch(eligibleForSearch: Boolean); cdecl;
        procedure setExpirationDate(expirationDate: NSDate); cdecl;
        procedure setKeywords(keywords: NSSet); cdecl;
        procedure setNeedsSave(needsSave: Boolean); cdecl;
        procedure setPersistentIdentifier(persistentIdentifier: NSUserActivityPersistentIdentifier); cdecl;
        procedure setReferrerURL(referrerURL: NSURL); cdecl;
        procedure setRequiredUserInfoKeys(requiredUserInfoKeys: NSSet); cdecl;
        procedure setSupportsContinuationStreams(supportsContinuationStreams: Boolean); cdecl;
        procedure setTargetContentIdentifier(targetContentIdentifier: NSString); cdecl;
        procedure setTitle(title: NSString); cdecl;
        procedure setUserInfo(userInfo: NSDictionary); cdecl;
        procedure setWebpageURL(webpageURL: NSURL); cdecl;
        function supportsContinuationStreams: Boolean; cdecl;
        function targetContentIdentifier: NSString; cdecl;
        function title: NSString; cdecl;
        function userInfo: NSDictionary; cdecl;
        function webpageURL: NSURL; cdecl;
      end;
      TNSUserActivity = class(TOCGenericImport<NSUserActivityClass, NSUserActivity>) end;
      
    ...  
    function NSUserActivityTypeBrowsingWeb: NSString;
    ...
    implementation
    ...
    function NSUserActivityTypeBrowsingWeb: NSString;
    begin
      result := CocoaNSStringConst(FoundationFwk, 'NSUserActivityTypeBrowsingWeb');
    end;

In the file **FMX.Platform.iOS.pas**, in the TApplicationDelegate class, in the private section, put the code:

      class function applicationContinueUserActivityRestorationHandler(self: id; _cmd: SEL; application: PUIApplication;
          userActivity: Pointer; restorationHandler: Pointer; restorableObjects: Pointer): Boolean; cdecl; static;
 
In the file **FMX.Platform.iOS.pas**, in the implementation of the method TApplicationDelegate.CreateDelegateMetaClass, before the line "objc_registerClassPair(DelegateClass);", put the code:

      class_addMethod(DelegateClass, sel_getUid('application:continueUserActivity:restorationHandler:'),
          @applicationContinueUserActivityRestorationHandler, 'B@:@@@@');

In the file **FMX.Platform.iOS.pas**, in the TApplicationDelegate implementation, put the code:

    class function TApplicationDelegate.applicationContinueUserActivityRestorationHandler(
      self: id; _cmd: SEL; application: PUIApplication; userActivity,
      restorationHandler, restorableObjects: Pointer): Boolean;
    var
      LUserActivity: NSUserActivity;
      LURLString: string;
    begin
      Result := False;
      if Assigned(userActivity) then
      begin
        LUserActivity := TNSUserActivity.Wrap(userActivity);
        if NSStrToStr(LUserActivity.activityType) = NSStrToStr(NSUserActivityTypeBrowsingWeb) then
        begin
          if Assigned(LUserActivity.webpageURL) then
            LURLString := NSStrToStr(LUserActivity.webpageURL.absoluteString)
          else
            LURLString := string.Empty;
    
          Result := PlatformCocoaTouch.HandleApplicationEvent(TApplicationEvent.OpenURL,
            TiOSOpenApplicationContext.Create(string.Empty, LURLString, nil));
        end;
      end;
    end;

## Usage

    uses
      System.Messaging,
      FMX.Platform,
      FMX.Platform.iOS,
      FMX.Dialogs;
    
    constructor TipUrlHandler.Create;
    begin
      inherited Create;
      TMessageManager.DefaultManager.SubscribeToMessage(TApplicationEventMessage, ApplicationEventMessageHandler);
    end;
    
    destructor TipUrlHandler.Destroy;
    begin
      TMessageManager.DefaultManager.Unsubscribe(TApplicationEventMessage, ApplicationEventMessageHandler, True);
      inherited;
    end;
    
    procedure TipUrlHandler.ApplicationEventMessageHandler(const ASender: TObject;
      const AMessage: TMessage);
    begin
      case TApplicationEventData(TApplicationEventMessage(AMessage).Value).Event of
        TApplicationEvent.OpenUrl:
          begin
            Showmessage(TiOSOpenApplicationContext(TApplicationEventData(TApplicationEventMessage(AMessage).Value).Context).URL);
          end;
      else
      end;
    end;
