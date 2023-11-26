Program JJProjGen;

{$mode objfpc}{$H+}

Uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, FrameViewer09,
  projgenMainform,
  projgengenform,
  projgenmeta,
  commonutils,
  projgenpreview, projgenhelpform;

{$R *.res}

Begin
  RequireDerivedFormResource := True;
  Application.Scaled         := True;
  Application.Initialize;
  Application.CreateForm(TProjGenForm, ProjGenForm);
  Application.CreateForm(TGenForm, GenForm);
  Application.CreateForm(THelpForm, HelpForm);
  Application.Run;
End.
