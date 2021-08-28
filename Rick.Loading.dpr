program Rick.Loading;

uses
  System.StartUpCopy,
  FMX.Forms,
  Rick.Loading.Interfaces in 'src\Rick.Loading.Interfaces.pas',
  Rick.Loading.Register in 'src\Rick.Loading.Register.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Run;
end.
