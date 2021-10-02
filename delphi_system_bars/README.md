# Firemonkey system bars

One of the difficulties that we have in firemonkey is to have more control of the system bars and this is so important for the design of the app that it should be done in a simple way via form properties, but as this is not yet possible I will teach here how to better configure the system bars (status bar and navigation bar), changing the background color or making them background full transparent, including on the splash screen, allowing a more beautiful app:

<p align="center">
<img src="screenshots/calculator.png" width=246 height=531> <img src="screenshots/gallery.png" width=246 height=531> <img src="screenshots/nubank.png" width=246 height=531>
</p>

## System bars

After added the units of this repository in your project, in your forms you can simple:

```delphi
uses
  iPub.FMX.SystemBars;

  ...

  Form1.SystemBars.StatusBarBackgroundColor := TAlphaColors.Black;
  Form1.SystemBars.NavigationBarBackgroundColor := TAlphaColors.Black;
```

But if you want to draw something underneath, like having an image in the background, you can change the visibility:

```delphi
uses
  iPub.FMX.SystemBars;

  ...

  Form1.SystemBars.StatusBarBackgroundColor := $20000000;
  Form1.SystemBars.NavigationBarBackgroundColor := $20000000;
  Form1.SystemBars.Visibility := TipFormSystemBars.TVisibilityMode.VisibleAndOverlap;
```

When TVisibilityMode.VisibleAndOverlap is being used, you can see the size of the system bars through the code:

```delphi
uses
  iPub.FMX.SystemBars;

  ...

  Form1.SystemBars.Visibility := TipFormSystemBars.TVisibilityMode.VisibleAndOverlap;
  Padding.Rect := Form1.SystemBars.Insets;
```

There is also the TappableInsets property, which are the distances in which the system bars, without gesture bar, are overlapping the sides of the form (top, left, right and bottom).

You can also capture changes to Insets. This can be useful when the app is rotated. See:

```delphi
uses
  iPub.FMX.SystemBars;

  ...

procedure TForm1.SystemBarsInsetsChange(Sender: TObject);
begin
end;

  Form1.SystemBars.OnSystemBarsInsetsChange := SystemBarsInsetsChange;
```

## Android Splash screen - System bars and background

The class TAndroidSystemBars is just to use in runtime, after the application full launch. But if you don't change the default "styles.xml" and "splash_image_def.xml" provided by Delphi, you will have an ugly system bars or simply not predictable (some devices may be white, others black or even gray), and the splash screen background will be always black. To fix the system bars colors and background color of splash screen you should change the "styles.xml" and "splash_image_def.xml" files.

### Creating "styles.xml"

Create one file called "styles.xml" with the following content:

  ```xml
  <resources xmlns:android="http://schemas.android.com/apk/res/android">
      <style name="AppTheme" parent="@android:style/Theme.Material.Light.NoActionBar">
          <item name="android:navigationBarColor">#01ffffff</item>
          <item name="android:statusBarColor">#00000000</item>
          <item name="android:colorPrimary">#ffffff</item>
          <item name="android:windowBackground">@drawable/splash_image_def</item>
      </style>
  </resources>
  ```

In this content above I used the white color example (#ffffff) but you can change it to your color but note that in navigationBarColor the color need to start with "#01" to set the alpha value to 1 to solve problems found in LG devices

Save the file in one subdirectory of your project, like "resources" subdirectory, for example.

### Creating "splash_image_def.xml"

Create one file called "splash_image_def.xml" with the following content:

  ```xml
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
  ```

In this content above I used one white color example (#ffffff) but you can change it to your color.

Save the file in one subdirectory of your project, like "resources" subdirectory, for example.

### Deploying the "styles.xml" and "splash_image_def.xml"

 1) Open your Project > Deployment, then you need to apply the following steps below for Android 32/64 bits in Release and Debug mode:
 2) Look in the "Local Name" column and disable these 3 default files: "styles.xml", "styles-v21.xml" and "splash_image_def.xml"
 3) Add your own "styles.xml" file and set the Remote Path field to "res\values"
 4) Add your own "splash_image_def.xml" file and set the Remote Path field to "res\drawable"

Now you can uninstall your app, compile and run to see the difference ;)

## Compatibility

I made some tests with Delphi 11 Alexandria, Delphi 10.4.2 Sydney and Delphi 10.3.3 Rio running the app in some devices:

    GOOGLE   - G020A Android64 11.0.0 (API level 30)
    LG       - LM-X430 Android32 10.0.0 (API level 29)
    SAMSUNG  - SM-A013M Android32 10.0.0 (API level 29)
    SAMSUNG  - SM-G955F Android64 9.0.0 (API level 28)
    SAMSUNG  - SM-G935F Android64 8.0.0 (API level 26)
    MOTOROLA - MotoG3 Android32 6.0.0 (API level 23)

All worked perfectly. But these codes should work well on all android versions supported by delphi (Android 6 to Android 11), although it is likely to work on older Android devices as well.
