unit RICK.Loading;

interface

uses
  System.SysUtils,
  System.UITypes,

  FMX.Ani,
  FMX.Forms,
  FMX.Objects,
  FMX.Layouts,
  FMX.StdCtrls,

  RICK.Loading.Interfaces, System.Classes;

type
  TRICKLoading = class(TInterfacedObject, iRICKLoading)
  private
    class var FMensage: String;
    class var FSourceName: string;
    class var FSourceSize: Integer;
    class var FSourceColor: TAlphaColor;
    class var FAnimationColor: TAlphaColor;
    class var FBackgroundColor: TAlphaColor;
    class var FBackground: TRectangle;
    class var FLayout: TLayout;
    class var FArc: TArc;
    class var FAnimation: TFloatAnimation;
    class var FLabel: TLabel;
    class var FOpacityBackground: Single;
    class var FOpacityAnimationText: Single;

    function Execute(const AProc: TProc): iRICKLoading; overload;
    function Execute(const AProc: TProc; ANotifyEvent: TNotifyEvent): iRICKLoading; overload;
    function DoMessage(const AValue: string): iRICKLoading;
    function ChangeMessage(const AValue: string): iRICKLoading;
    function SourceSize(const AValue: Integer): iRICKLoading;
    function SourceName(const AValue: string): iRICKLoading;
    function SourceColor(Const AValue: TAlphaColor): iRICKLoading;
    function AnimationColor(Const AValue: TAlphaColor): iRICKLoading;
    function BackgroundColor(Const AValue: TAlphaColor): iRICKLoading;
    function OpacityBackground(Const AValue: Single): iRICKLoading;
    function OpacityAnimationText(Const AValue: Single): iRICKLoading;
    procedure DestroyAnimation;

    class procedure ConstructAnimation;

    constructor Create;

  public
    destructor Destroy; override;
    class function New: iRICKLoading;
  end;

implementation

uses
  FMX.Types,
  FMX.Graphics,
  FMX.Platform,
  FMX.VirtualKeyboard;

function TRICKLoading.AnimationColor(const AValue: TAlphaColor): iRICKLoading;
begin
  Result:= Self;

  FAnimationColor:= AValue;
end;

function TRICKLoading.SourceColor(const AValue: TAlphaColor): iRICKLoading;
begin
  Result := Self;
  FSourceColor := AValue;
end;

function TRICKLoading.BackgroundColor(const AValue: TAlphaColor): iRICKLoading;
begin
  Result:= Self;
  FBackgroundColor:= AValue;
end;

constructor TRICKLoading.Create;
begin
  FMensage := 'Carregando...';
  FSourceName := 'Segoe UI';
  FSourceSize := 16;
  FSourceColor := TAlphaColorRec.White;
  FAnimationColor:= TAlphaColorRec.White;
  FBackgroundColor:= TAlphaColorRec.Black;
  FOpacityBackground:= 0.7;
  FOpacityAnimationText:= 1;
end;

class procedure TRICKLoading.ConstructAnimation;
var
  FService: IFMXVirtualKeyboardService;
begin

  // Panel de FBackground opaco...
  FBackground := TRectangle.Create(Screen.ActiveForm);
  FBackground.Opacity := 0;
  FBackground.Parent := Screen.ActiveForm;
  FBackground.Visible := true;
  FBackground.Align := TAlignLayout.Contents;
  FBackground.Fill.Color := FBackgroundColor;
  FBackground.Fill.Kind := TBrushKind.Solid;
  FBackground.Stroke.Kind := TBrushKind.None;
  FBackground.Visible := true;

  // FLayout contendo o texto e o FArc...
  FLayout := TLayout.Create(Screen.ActiveForm);
  FLayout.Opacity := 0;
  FLayout.Parent := Screen.ActiveForm;
  FLayout.Visible := true;
  FLayout.Align := TAlignLayout.Contents;
  FLayout.Width := 250;
  FLayout.Height := 78;
  FLayout.Visible := true;

  // Arco da animacao...
  FArc := TArc.Create(Screen.ActiveForm);
  FArc.Visible := true;
  FArc.Parent := FLayout;
  FArc.Align := TAlignLayout.Center;
  FArc.Margins.Bottom := 55;
  FArc.Width := 25;
  FArc.Height := 25;
  FArc.EndAngle := 280;
  FArc.Stroke.Color := FAnimationColor;
  FArc.Stroke.Thickness := 2;
  FArc.Position.X := trunc((FLayout.Width - FArc.Width) / 2);
  FArc.Position.Y := 0;

  // Animacao...
  FAnimation := TFloatAnimation.Create(Screen.ActiveForm);
  FAnimation.Parent := FArc;
  FAnimation.StartValue := 0;
  FAnimation.StopValue := 360;
  FAnimation.Duration := 0.8;
  FAnimation.Loop := true;
  FAnimation.PropertyName := 'RotationAngle';
  FAnimation.AnimationType := TAnimationType.InOut;
  FAnimation.Interpolation := TInterpolationType.Linear;
  FAnimation.Start;

  // Label do texto (DoMensage)...
  FLabel := TLabel.Create(Screen.ActiveForm);
  FLabel.Parent := FLayout;
  FLabel.Align := TAlignLayout.Center;
  FLabel.Margins.Top := 60;
  FLabel.Font.Size := FSourceSize;
  FLabel.Height := 70;
  FLabel.Width := FBackground.Width - 100;
  FLabel.AutoSize:= True;
  FLabel.FontColor := FSourceColor;
  FLabel.TextSettings.HorzAlign := TTextAlign.Center;
  FLabel.TextSettings.VertAlign := TTextAlign.Leading;
  FLabel.StyledSettings := [TStyledSetting.Family, TStyledSetting.Style];
  FLabel.Text := FMensage;
  FLabel.VertTextAlign := TTextAlign.Leading;
  FLabel.Trimming := TTextTrimming.None;
  FLabel.TabStop := False;
  FLabel.SetFocus;

  // Exibe os controles... 
  TAnimator.AnimateFloat(FBackground, 'Opacity', FOpacityBackground);
  TAnimator.AnimateFloat(FLayout, 'Opacity', FOpacityAnimationText);

  FLayout.BringToFront;

  // Esconde o teclado virtual...
  TPlatformServices.Current.SupportsPlatformService(IFMXVirtualKeyboardService,
    IInterface(FService));
  if (FService <> nil) then
  begin
    FService.HideVirtualKeyboard;
  end;
  FService := nil;
end;

destructor TRICKLoading.Destroy;
begin

  inherited;
end;

procedure TRICKLoading.DestroyAnimation;
begin
  if Assigned(FLayout) then
  begin

    try
      if Assigned(FLabel) then
        FLabel.DisposeOf;

      if Assigned(FAnimation) then
        FAnimation.DisposeOf;

      if Assigned(FArc) then
        FArc.DisposeOf;

      if Assigned(FBackground) then
        FBackground.DisposeOf;

      if Assigned(FLayout) then
        FLayout.DisposeOf;

    except
    end;
  end;

  FLabel := nil;
  FAnimation := nil;
  FArc := nil;
  FLayout := nil;
  FBackground := nil;
end;

function TRICKLoading.Execute(const AProc: TProc): iRICKLoading;
begin
  Result := Self;
  ConstructAnimation;

  TThread.CreateAnonymousThread(
    procedure
    begin
      try
        AProc;
      finally
        TThread.Synchronize(nil,
          procedure
          begin
            DestroyAnimation;
          end);
      end;
    end).Start;

end;

function TRICKLoading.DoMessage(const AValue: string): iRICKLoading;
begin
  Result := Self;
  FMensage := AValue;
end;

function TRICKLoading.Execute(const AProc: TProc;
  ANotifyEvent: TNotifyEvent): iRICKLoading;
var
  LThread: TThread;
begin
  Result := Self;
  ConstructAnimation;

  LThread:= TThread.CreateAnonymousThread(
    procedure
    begin
      AProc;
    end);

  LThread.FreeOnTerminate:= True;
  LThread.OnTerminate:= ANotifyEvent;
  LThread.Start;
end;

function TRICKLoading.ChangeMessage(const AValue: string): iRICKLoading;
begin
  Result:= Self;

  if Assigned(FLayout) then
  begin
    try
      if Assigned(FLabel) then
        FLabel.Text := AValue;
    except
    end;
  end;

end;

class function TRICKLoading.New: iRICKLoading;
begin
  Result := Self.Create;
end;

function TRICKLoading.SourceName(const AValue: string): iRICKLoading;
begin
  Result := Self;
  FSourceName := AValue;
end;

function TRICKLoading.OpacityAnimationText(const AValue: Single): iRICKLoading;
begin
  Result:= Self;
  FOpacityAnimationText:= AValue;
end;

function TRICKLoading.OpacityBackground(const AValue: Single): iRICKLoading;
begin
  Result:= Self;
  FOpacityBackground:= AValue;
end;

function TRICKLoading.SourceSize(const AValue: Integer): iRICKLoading;
begin
  Result := Self;
  FSourceSize := AValue;
end;

end.
