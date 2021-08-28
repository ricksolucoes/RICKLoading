unit Rick.Loading.Interfaces;

interface

uses
  System.SysUtils,
  System.UITypes,

  FMX.Forms;

type
  iLoading = interface
    ['{B2B9D25B-20AF-4F7D-8C1F-2E110D9B858F}']
    function Execute(const AProc: TProc): iLoading;
    function DoMessage(const AValue: string): iLoading;
    function ChangeMessage(const AValue: string): iLoading;
    function SourceSize(const AValue: Integer): iLoading;
    function SourceName(const AValue: string): iLoading;
    function SourceColor(const AValue: TAlphaColor): iLoading;
    function AnimationColor(Const AValue: TAlphaColor): iLoading;
    function BackgroundColor(Const AValue: TAlphaColor): iLoading;
    function OpacityBackground(Const AValue: Single): iLoading;
    function OpacityAnimationText(Const Value: Single): iLoading;


  end;

implementation

end.
