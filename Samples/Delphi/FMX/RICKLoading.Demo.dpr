program RICKLoading.Demo;

uses
  System.StartUpCopy,
  FMX.Forms,
  RICK.Loading.View.Main in 'RICK.Loading.View.Main.pas' {Form1};

{$R *.res}

begin
{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown:= True;
{$ENDIF}

  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
