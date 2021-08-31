program RICKLoading;

uses
  System.StartUpCopy,
  FMX.Forms,
  RICK.Loading.Interfaces in 'src\RICK.Loading.Interfaces.pas',
  RICK.Loading in 'src\RICK.Loading.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Run;
end.
