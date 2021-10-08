unit RICK.Loading.Interfaces;

interface

uses
  System.SysUtils,
  System.UITypes,

  FMX.Forms, System.Classes;

type
  iRICKLoading = interface
    ['{B2B9D25B-20AF-4F7D-8C1F-2E110D9B858F}']
    function Execute(const AProc: TProc): iRICKLoading; overload;
    function Execute(const AProc: TProc; ANotifyEvent: TNotifyEvent): iRICKLoading; overload;
    function DoMessage(const AValue: string): iRICKLoading;
    function ChangeMessage(const AValue: string): iRICKLoading;
    function SourceSize(const AValue: Integer): iRICKLoading;
    function SourceName(const AValue: string): iRICKLoading;
    function SourceColor(const AValue: TAlphaColor): iRICKLoading;
    function AnimationColor(Const AValue: TAlphaColor): iRICKLoading;
    function BackgroundColor(Const AValue: TAlphaColor): iRICKLoading;
    function OpacityBackground(Const AValue: Single): iRICKLoading;
    function OpacityAnimationText(Const AValue: Single): iRICKLoading;
    procedure DestroyAnimation;


  end;

implementation

end.
