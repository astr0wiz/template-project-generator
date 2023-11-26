Unit projgenGenform;

{$mode objfpc}{$H+}

Interface

Uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  ComCtrls,
  StdCtrls,
  ButtonPanel,
  ExtCtrls,
  Menus,
  regexpr,
  projgenmeta,
  projgenpreview,
  commonutils,
  Types,
  graphtype,
  fileutil;

Type

  { TTokenEntry }

  TTokenEntry = Record
    Token:        string;
    Substitution: string;
  End;

  TTokenEntries = Array Of TTokenEntry;

  TFileGroupType   = (fgFolder, fgSource, fgResource, fgImage, fgSound, fgOption, fgUnknown);
  TStructureImages = (siFolder, siAppSource, siPackage, siPascalSource, siForm,
    siResource, siImage, siSound, siIni, siUnknown, siIgnored,
    siAppMain, siBlank, siInclude);

  {  TFileEntity  }

  TFileEntity = Class
    GroupType:          TFileGroupType;
    Ignore:             boolean;
    CanTokenizeInnards: boolean;
    DestPath:           string;
    SrcPath:            string;
    SrcName:            string;
    OrgImageIndex:      integer;
    CustomDestName:     string;
  End;

  { TGenForm }

  TGenForm = Class(TForm)
    ButtonPanel1:  TButtonPanel;
    FileImageList: TImageList;
    Label1:        TLabel;
    Label2:        TLabel;
    Label3:        TLabel;
    FileStructure: TTreeView;
    DirLocation:   TPanel;
    Label4:        TLabel;
    MenuSeparator: TMenuItem;
    MenuNewFolder: TMenuItem;
    MenuPreview:   TMenuItem;
    MenuMeta:      TMenuItem;
    MenuRename:    TMenuItem;
    MenuIgnore:    TMenuItem;
    StructureMenu: TPopupMenu;
    Procedure FileStructureContextPopup(Sender: TObject; MousePos: TPoint; Var Handled: boolean);
    Procedure FileStructureDeletion(Sender: TObject; Node: TTreeNode);
    Procedure FormClose(Sender: TObject; Var CloseAction: TCloseAction);
    Procedure MenuIgnoreClick(Sender: TObject);
    Procedure MenuMetaClick(Sender: TObject);
    Procedure MenuNewFolderClick(Sender: TObject);
    Procedure MenuPreviewClick(Sender: TObject);
    Procedure MenuRenameClick(Sender: TObject);
    Procedure OKButtonClick(Sender: TObject);
  Private
    Function Tokenize(rawValue: string): string;
    Procedure SetIgnore(Anode: TTreeNode; IgnoreFlag: boolean);
    Procedure SetIgnoreFromParent(Anode: TTreeNode; IgnoreFlag: boolean);
    Function GenerateDestPath(Anode: TTreeNode): string;
    Procedure GenerateDestinationProject(Anode: TTreeNode);
    Function GenerateDestinationFolder(FullPath: string): boolean;
    Function GenerateDestinationFile(FileData: TFileEntity): boolean;
    Function FinalDestinationName(FileData: TFileEntity): string;
  Public
    Procedure AddToken(NewToken: string; NewSub: string);
    Procedure SetDirLocation(Value: string);
    Procedure DiscoverFileStructure(DestDirName: string; SourceDir: string; ParentNode: TTreeNode);
  End;

Var
  GenForm:     TGenForm;
  MetaForm:    TMetaForm;
  PreviewForm: TPreviewForm;

Implementation

Var
  TokenList:  TTokenEntries;
  ActiveNode: TTreeNode;

{$R *.lfm}

{ TGenForm }


Procedure TGenForm.FileStructureDeletion(Sender: TObject; Node: TTreeNode);
  Begin
    If Assigned(Node.Data) Then
      Begin
      TFileEntity(Node.Data).Free;
      Node.Data := nil;
      End;
  End;

Procedure TGenForm.FormClose(Sender: TObject; Var CloseAction: TCloseAction);
  Begin
    SetLength(TokenList, 0);
    CloseAction := TCloseAction.caFree;
  End;


Procedure TGenForm.MenuIgnoreClick(Sender: TObject);
  Var
    isIgnore: boolean;
  Begin
    TFileEntity(ActiveNode.Data).Ignore := Not TFileEntity(ActiveNode.Data).Ignore;
    If TFileEntity(ActiveNode.Data).Ignore Then
      ActiveNode.ImageIndex := Ord(siIgnored)
    Else
      ActiveNode.ImageIndex := TFileEntity(ActiveNode.Data).OrgImageIndex;
    // Now see if this is a folder
    isIgnore := TFileEntity(ActiveNode.Data).Ignore;
    If TFileEntity(ActiveNode.Data).GroupType = fgFolder Then
      If ActiveNode.HasChildren Then
        SetIgnoreFromParent(ActiveNode, isIgnore);
  End;

Procedure TGenForm.FileStructureContextPopup(Sender: TObject; MousePos: TPoint; Var Handled: boolean);
  Begin
    Handled    := False;
    ActiveNode := FileStructure.GetNodeAt(MousePos.X, MousePos.Y);
    If ActiveNode = nil Then
      Handled := True
    Else
      Begin
      If ActiveNode.Level = 0 Then
        Begin
        MenuMeta.Visible      := False;
        MenuPreview.Visible   := False;
        MenuRename.Visible    := False;
        MenuIgnore.Visible    := False;
        MenuSeparator.Visible := False;
        End
      Else
        Begin
        MenuMeta.Visible      := True;
        MenuPreview.Visible   := True;
        MenuRename.Visible    := False; // Only true for a few cases
        MenuIgnore.Visible    := True;
        MenuSeparator.Visible := True;
        End;
      Case TFileEntity(ActiveNode.Data).GroupType Of
        fgFolder:
          Begin
          MenuNewFolder.Visible := True;
          MenuPreview.Visible   := False;
          End;
        Else
          Begin
          MenuNewFolder.Visible := False;
          MenuPreview.Visible   := True;
          MenuRename.Visible    := True;
          MenuPreview.Enabled   := TFileEntity(ActiveNode.Data).CanTokenizeInnards;
          End;
        End;
      If TFileEntity(ActiveNode.Data).Ignore Then
        Begin
        MenuIgnore.Caption := 'Include';
        MenuIgnore.Enabled := Not TFileEntity(ActiveNode.Parent.Data).Ignore;
        End
      Else
        MenuIgnore.Caption := 'Ignore';
      End;
  End;


Procedure TGenForm.MenuMetaClick(Sender: TObject);
  Begin
    MetaForm := TMetaForm.Create(self);
      Try
      With MetaForm Do
        Begin
        Caption                  := 'Metadata for ' + ActiveNode.Text;
        FileNameOriginal.Caption := TFileEntity(ActiveNode.Data).SrcName;
        DirectorySource.Caption  := TFileEntity(ActiveNode.Data).SrcPath;
        DirectoryDestination.Caption := IncludeTrailingPathDelimiter(DirLocation.Caption) + TFileEntity(ActiveNode.Data).DestPath;
        If TFileEntity(ActiveNode.Data).CanTokenizeInnards Then
          CanTokenizeContents.Caption := 'Yes'
        Else
          CanTokenizeContents.Caption := 'No';
        FileNameTokenized.Caption := Tokenize(TFileEntity(ActiveNode.Data).SrcName);
        ShowModal;
        End;
      Finally
      FreeAndNil(MetaForm);
      End;
  End;

Procedure TGenForm.MenuNewFolderClick(Sender: TObject);
  Var
    suggested: string;
    anode:     TTreeNode;
    sameCount: integer;
    entity:    TFileEntity;
    n:         TTreeNode;
  Begin
    suggested := 'NewFolder';
    sameCount := 0;

    If ActiveNode.HasChildren Then
      Begin
      anode := ActiveNode.GetFirstChild;
      // ------ extract next three into new function returning 0 or 1 to add to sameCount
      While Assigned(anode) Do
        Begin
        If TFileEntity(anode.Data).GroupType = fgFolder Then
          If ExecRegExpr('(?i)^newfolder[0-9]*$', anode.Text) Then
            Inc(sameCount);
        anode := anode.GetNextSibling;
        End;
      End;
    If sameCount > 0 Then
      suggested := suggested + IntToStr(sameCount + 1);
    If InputQuery('New Folder', 'Enter a new folder name', suggested) Then
        Try
        n                    := FileStructure.Items.AddChild(ActiveNode, suggested);
        entity               := TFileEntity.Create;
        entity.CanTokenizeInnards := False;
        entity.SrcPath       := IncludeTrailingPathDelimiter(TFileEntity(ActiveNode.Data).SrcPath) + suggested;
        entity.SrcName       := suggested;
        entity.DestPath      := GenerateDestPath(n);
        entity.GroupType     := fgFolder;
        entity.OrgImageIndex := Ord(siFolder);
        n.ImageIndex         := Ord(siFolder);
        n.Data               := entity;
        Finally
        entity := nil;
        End;
  End;

Procedure TGenForm.MenuPreviewClick(Sender: TObject);
  Var
    fdata:  TFileEntity;
    Source: string;
    F:      TextFile;
    s:      string;
  Begin
    PreviewForm := TPreviewForm.Create(self);
      Try
      fdata  := TFileEntity(ActiveNode.Data);
      Source := IncludeTrailingPathDelimiter(fdata.SrcPath) + fdata.SrcName;
      AssignFile(F, Source);
      reset(F);
      Repeat
        ReadLn(F, s);
        If fdata.CanTokenizeInnards Then
          PreviewForm.DisplayPane.Append(Tokenize(s))
        Else
          PreviewForm.DisplayPane.Append(s);
      Until EOF(F);
      CloseFile(F);
      PreviewForm.ShowModal;
      Finally
      FreeAndNil(PreviewForm);
      End;
  End;

Procedure TGenForm.MenuRenameClick(Sender: TObject);
  Var
    s: string;
    e: TFileEntity;
  Begin
    (*
    -------------------------------------- PROBLEM ------------------------------------------

    When a folder is renamed, the DestPath of ALL items under it gets hosed.  So, we either
    need to re-calculate all dest paths under the directory or figure out a better way to
    generate the destination path when the files and folders are generated.

    -----------------------------------------------------------------------------------------
    *)
    s := ActiveNode.Text;
    If InputQuery('Rename', 'Enter new name and press OK to save', s) Then
      If IsValidFilesystemName(s) Then
        Begin
        ActiveNode.Text  := s;
        e                := TFileEntity(ActiveNode.Data);
        e.CustomDestName := s;
        If ActiveNode.Level > 0 Then
          If e.GroupType = fgFolder Then
            e.DestPath := GenerateDestpath(ActiveNode);
        End
      Else
        MessageDlg('Rename Error', Format('"%s" is an invalid file name!', [s]), mtWarning, [mbClose], 0);
  End;

Procedure TGenForm.OKButtonClick(Sender: TObject);
  Var
    searchInfo: TSearchRec;
    isEmpty:    boolean;
    rx:         TRegExpr;
    ok2go:      boolean;
  Begin
    rx      := TRegExpr.Create('^\.|\.\.$');
    ok2go   := True;
    isEmpty := True;
      Try
      If FindFirst(IncludeTrailingPathDelimiter(DirLocation.Caption) + '*', faDirectory, searchInfo) = 0 Then
        Repeat
          isEmpty := rx.Exec(searchInfo.Name);
        Until Not isEmpty Or (FindNext(searchInfo) <> 0);
      Finally
      FindClose(searchInfo);
      rx.Free;
      End;
    If Not isEmpty Then
      ok2go := MessageDlg('File Generation Warning', 'The destination directory is not empty.  Files may be overwritten.  Press "ok" to continue anyway.', mtWarning, [mbOK, mbAbort], 0, mbAbort) = mrOk;
    If isEmpty Or ok2go Then
      GenerateDestinationProject(FileStructure.Items[0]);
  End;


Function TGenForm.Tokenize(rawValue: string): string;
  Var
    i: integer;
  Begin
    Result := rawValue;
    If Length(Result) > 0 Then
      For i := 0 To Length(TokenList) - 1 Do
        Begin
        Result := ReplaceRegExpr(TokenList[i].Token, Result, TokenList[i].Substitution);
        End;
  End;

Procedure TGenForm.SetIgnore(Anode: TTreeNode; IgnoreFlag: boolean);
  Begin
    TFileEntity(Anode.Data).Ignore := IgnoreFlag;
    If IgnoreFlag Then
      Anode.ImageIndex := Ord(siIgnored)
    Else
      Anode.ImageIndex := TFileEntity(Anode.Data).OrgImageIndex;
  End;

Procedure TGenForm.SetIgnoreFromParent(Anode: TTreeNode; IgnoreFlag: boolean);
  Var
    child: TTreeNode;
  Begin
    child := Anode.GetFirstChild;
    While child <> nil Do
      Begin
      SetIgnore(child, IgnoreFlag);
      If TFileEntity(child.Data).GroupType = fgFolder Then
        Begin
        SetIgnoreFromParent(child, IgnoreFlag);
        End;
      child := child.GetNextSibling;
      End;
  End;

Function TGenForm.GenerateDestPath(Anode: TTreeNode): string;
  Begin
    Result := '';
    If Assigned(Anode) Then
      Begin
      If Anode.Level > 1 Then
        Result := GenerateDestPath(Anode.Parent);
      Result   := Result + IncludeTrailingPathDelimiter(Anode.Text);
      End;
  End;

Procedure TGenForm.GenerateDestinationProject(Anode: TTreeNode);
  Var
    child: TTreeNode;
  Begin
    If TFileEntity(Anode.Data).GroupType = fgFolder Then
      If Not GenerateDestinationFolder(IncludeTrailingPathDelimiter(DirLocation.Caption) + TFileEntity(Anode.Data).DestPath) Then
        Begin
        ShowMessage(format('ERROR: Folder %s could not be created!', [TFileEntity(Anode.Data).DestPath]));
        exit;
        End;
    child := Anode.GetFirstChild;
    While child <> nil Do
      Begin
      If Not TFileEntity(child.Data).Ignore Then
        If TFileEntity(child.Data).GroupType = fgFolder Then
          GenerateDestinationProject(child)
        Else
          GenerateDestinationFile(TFileEntity(child.Data));
      child := child.GetNextSibling;
      End;
  End;

Function TGenForm.GenerateDestinationFolder(FullPath: string): boolean;
  Begin
    If Not DirectoryExists(FullPath) Then
      CreateDir(FullPath);
    Result := True;
  End;

Function TGenForm.GenerateDestinationFile(FileData: TFileEntity): boolean;
  Var
    Source:        string;
    destination:   string;
    FRead, FWrite: TextFile;
    s:             string;
  Begin
    Result := False;
    Source := IncludeTrailingPathDelimiter(FileData.SrcPath) + FileData.SrcName;
    If FileData.DestPath = '' Then
      destination := IncludeTrailingPathDelimiter(DirLocation.Caption) + FinalDestinationName(FileData)
    Else
      destination := IncludeTrailingPathDelimiter(DirLocation.Caption) + IncludeTrailingPathDelimiter(FileData.DestPath) + FinalDestinationName(FileData);
    If FileExists(destination) Then
      Begin
      ShowMessage(Format('ERROR: File "%s" already exists!', [destination]));
      Exit;
      End
    Else
      If FileData.CanTokenizeInnards Then
        Begin
        AssignFile(FRead, Source);
        AssignFile(FWrite, destination);
        reset(FRead);
        rewrite(FWrite);
        Repeat
          ReadLn(FRead, s);
          WriteLn(FWrite, Tokenize(s))
        Until EOF(FRead);
        CloseFile(FRead);
        CloseFile(FWrite);
        End
      Else
        CopyFile(Source, destination);
    Result := True;
  End;

Function TGenForm.FinalDestinationName(FileData: TFileEntity): string;
  Begin
    Result := FileData.CustomDestName;
    If Length(FileData.CustomDestName) = 0 Then
      Result := Tokenize(FileData.SrcName);
  End;

Procedure TGenForm.AddToken(NewToken: string; NewSub: string);
  Begin
    SetLength(TokenList, Length(TokenList) + 1);
    With TokenList[Length(TokenList) - 1] Do
      Begin
      Token        := NewToken;
      Substitution := NewSub;
      End;
  End;

Procedure TGenForm.SetDirLocation(Value: string);
  Begin
    DirLocation.Caption := Value;
    DirLocation.Caption := Value;
    DirLocation.Hint    := Value;
  End;

Procedure TGenForm.DiscoverFileStructure(DestDirName: string; SourceDir: string; ParentNode: TTreeNode);


  Var
    n, nn:      TTreeNode;
    fulldir:    string;
    searchInfo: TSearchRec;
    rx:         TRegExpr;
    imageIdx:   integer;
    entity:     TFileEntity;
  Begin
    If FileStructure.Items.Count = 0 Then
      n := FileStructure.Items.Add(ParentNode, Tokenize(DestDirName))
    Else
      n := FileStructure.Items.AddChild(ParentNode, Tokenize(DestDirName));
    n.ImageIndex := Ord(siFolder);
    fulldir := IncludeTrailingPathDelimiter(SourceDir);
      Try
      entity                    := TFileEntity.Create;
      entity.CanTokenizeInnards := False;
      entity.SrcPath            := fulldir;
      entity.SrcName            := DestDirName;
      entity.Destpath           := '';
      If n.Level > 0 Then
        entity.DestPath := GenerateDestPath(n);
      entity.GroupType  := fgFolder;
      n.Data            := entity;
      rx                := TRegExpr.Create;
      rx.Expression     := '^(.+)\.([a-zA-Z0-9_]+)$';
      If FindFirst(fulldir + '*', faDirectory, searchInfo) = 0 Then
        Repeat
          If searchInfo.Attr And faDirectory = faDirectory Then
            Begin
            If (CompareText(searchInfo.Name, '.') <> 0) And (CompareText(searchInfo.Name, '..') <> 0) Then
              DiscoverFileStructure(searchInfo.Name, fulldir + Tokenize(searchInfo.Name), n);
            End
          Else
            If rx.Exec(searchInfo.Name) Then
              If CompareText(searchInfo.Name, 'Template.ini') <> 0 Then
                Begin
                entity                    := TFileEntity.Create;
                entity.CanTokenizeInnards := True;
                entity.SrcPath            := fulldir;
                entity.SrcName            := searchInfo.Name;
                If n.level > 1 Then
                  entity.DestPath := GenerateDestPath(n);
                Case LowerCase(rx.Match[2]) Of
                  'lpi':
                    Begin
                    imageIdx         := Ord(siAppSource); // project information
                    entity.GroupType := fgSource;
                    End;
                  'lps':
                    Begin
                    imageIdx         := Ord(siBlank); // project session (not sure if needed)
                    entity.GroupType := fgSource;
                    End;
                  'lpr':
                    Begin
                    imageIdx         := Ord(siAppMain); // project main source
                    entity.GroupType := fgSource;
                    End;
                  'pkg':
                    Begin
                    imageIdx         := Ord(siPackage); // package
                    entity.GroupType := fgSource;
                    End;
                  'pas', 'pp':
                    Begin
                    imageIdx         := Ord(siPascalSource); // pascal source
                    entity.GroupType := fgSource;
                    End;
                  'inc':
                    Begin
                    imageIdx         := Ord(siInclude); // include file
                    entity.GroupType := fgSource;
                    End;
                  'lfm':
                    Begin
                    imageIdx         := Ord(siForm); // form
                    entity.GroupType := fgSource;
                    End;
                  'res':
                    Begin
                    imageIdx                  := Ord(siResource); // resources
                    entity.CanTokenizeInnards := False;
                    entity.GroupType          := fgResource;
                    End;
                  'png', 'jpg', 'jpeg', 'bmp', 'ico', 'gif':
                    Begin
                    imageIdx                  := Ord(siImage);  // images
                    entity.CanTokenizeInnards := False;
                    entity.GroupType          := fgImage;
                    End;
                  'wav', 'ogg', 'mp3', 'mp4':
                    Begin
                    imageIdx                  := Ord(siSound);  // sound
                    entity.CanTokenizeInnards := False;
                    entity.GroupType          := fgSound;
                    End;
                  'ini': imageIdx := Ord(siIni); // options files
                  Else
                    // cannot use
                    imageIdx                  := Ord(siUnknown);
                    entity.GroupType          := fgUnknown;
                    entity.CanTokenizeInnards := False;
                  End;
                entity.OrgImageIndex := imageIdx;
                nn                   := FileStructure.Items.AddChild(n, Tokenize(searchInfo.Name));
                nn.ImageIndex        := imageIdx;
                nn.Data              := entity;
                End;
        Until FindNext(searchInfo) <> 0;
      Finally
      FindClose(searchInfo);
      rx.Free;
      entity := nil;
      End;
  End;

End.
