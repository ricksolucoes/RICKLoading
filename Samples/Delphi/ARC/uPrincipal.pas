unit uPrincipal;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Ani,
  FMX.Objects;

type
  TForm1 = class(TForm)
    arcMenor: TArc;
    faniMenor: TFloatAnimation;
    arcMaior: TArc;
    faniMaior: TFloatAnimation;
    arcCobertura: TArc;
    faniCobertura: TFloatAnimation;
    recFundo: TRectangle;
    procedure FormCreate(Sender: TObject);
    procedure faniCoberturaFinish(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.faniCoberturaFinish(Sender: TObject);
begin
  if faniCobertura.Tag = 1 then
  begin
    faniCobertura.StartValue:= 0;
    faniCobertura.StopValue:= 360;
    faniCobertura.Tag:= 0;

  end
  else
  begin
    faniCobertura.StartValue:= -358;
    faniCobertura.StopValue:= 0;
    faniCobertura.Tag:= 1;
  end;

  faniCobertura.Start;

end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  faniMenor.Start;
  faniMaior.Start;
  faniCobertura.Start;
end;

end.
