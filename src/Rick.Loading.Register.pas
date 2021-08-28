unit Rick.Loading.Register;

interface

uses
  System.SysUtils,
  System.UITypes,

  FMX.Ani,
  FMX.Forms,
  FMX.Objects,
  FMX.Layouts,
  FMX.StdCtrls,

  Rick.Loading.Interfaces;

type
  TLoading = class(TInterfacedObject, iLoading)
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

    function Execute(const AProc: TProc): iLoading;
    function DoMessage(const AValue: string): iLoading;
    function ChangeMessage(const AValue: string): iLoading;
    function SourceSize(const AValue: Integer): iLoading;
    function SourceName(const AValue: string): iLoading;
    function SourceColor(Const AValue: TAlphaColor): iLoading;
    function AnimationColor(Const AValue: TAlphaColor): iLoading;
    function BackgroundColor(Const AValue: TAlphaColor): iLoading;
    function OpacityBackground(Const AValue: Single): iLoading;
    function OpacityAnimationText(Const AValue: Single): iLoading;

    class procedure ConstructAnimation;
    class procedure DestroyAnimation;

    constructor Create;

  public
    destructor Destroy; override;
    class function New: iLoading;
  end;

implementation

uses
  FMX.Types,
  FMX.Graphics,
  FMX.Platform,
  FMX.VirtualKeyboard,

  System.Classes;

function TLoading.AnimationColor(const AValue: TAlphaColor): iLoading;
begin
  Result:= Self;

  FAnimationColor:= AValue;
end;

function TLoading.SourceColor(const AValue: TAlphaColor): iLoading;
begin
  Result := Self;
  FSourceColor := AValue;
end;

function TLoading.BackgroundColor(const AValue: TAlphaColor): iLoading;
begin
  Result:= Self;
  FBackgroundColor:= AValue;
end;

constructor TLoading.Create;
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

class procedure TLoading.ConstructAnimation;
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

destructor TLoading.Destroy;
begin

  inherited;
end;

class procedure TLoading.DestroyAnimation;
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

function TLoading.Execute(const AProc: TProc): iLoading;
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

function TLoading.DoMessage(const AValue: string): iLoading;
begin
  Result := Self;
  FMensage := AValue;
end;

function TLoading.ChangeMessage(const AValue: string): iLoading;
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

class function TLoading.New: iLoading;
begin
  Result := Self.Create;
end;

function TLoading.SourceName(const AValue: string): iLoading;
begin
  Result := Self;
  FSourceName := AValue;
end;

function TLoading.OpacityAnimationText(const AValue: Single): iLoading;
begin
  Result:= Self;
  FOpacityAnimationText:= AValue;
end;

function TLoading.OpacityBackground(const AValue: Single): iLoading;
begin
  Result:= Self;
  FOpacityBackground:= AValue;
end;

function TLoading.SourceSize(const AValue: Integer): iLoading;
begin
  Result := Self;
  FSourceSize := AValue;
end;

end.
