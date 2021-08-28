program RI2BCarregando.Demostracao;

uses
  System.StartUpCopy,
  FMX.Forms,
  RIBCarregar.View.Demostracao in 'RIBCarregar.View.Demostracao.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
