unit Rick.Loading.Interfaces;

interface

uses
  System.SysUtils,
  System.UITypes,

  FMX.Forms;

type
  iRickLoading = interface
    ['{B2B9D25B-20AF-4F7D-8C1F-2E110D9B858F}']
    function Execute(const AProc: TProc): iRickLoading;
    function DoMessage(const AValue: string): iRickLoading;
    function ChangeMessage(const AValue: string): iRickLoading;
    function SourceSize(const AValue: Integer): iRickLoading;
    function SourceName(const AValue: string): iRickLoading;
    function SourceColor(const AValue: TAlphaColor): iRickLoading;
    function AnimationColor(Const AValue: TAlphaColor): iRickLoading;
    function BackgroundColor(Const AValue: TAlphaColor): iRickLoading;
    function OpacityBackground(Const AValue: Single): iRickLoading;
    function OpacityAnimationText(Const Value: Single): iRickLoading;


  end;

implementation

end.
