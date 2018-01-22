unit GroupBoxEx;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls;

type
    TGroupBoxCaptionPos = ( gbTopLeft, gbTopMiddle, gbTopRight,
                            gbBottomLeft, gbBottomMiddle, gbBottomRight,
                            gbLeftTop, gbLeftMiddle, gbLeftBottom,
                            gbRightTop, gbRightMiddle, gbRightBottom );

    // Custom procedures
    TGroupBoxEnableDisableQuery = procedure (Sender: TObject; Control: TControl; Enabled: Boolean; var Handled: Boolean) of Object;
    TGroupBoxPaintCaptionBackground = procedure (Sender: TObject; Canvas: TCanvas; const Rect: TRect; var Handled: Boolean) of Object;

    TGroupBoxOptions = class (TPersistent)
    private
        fOnChange: TNotifyEvent;
        fCaptionSpaces: Boolean;
        fShowBorder: Boolean;
        fCaptionFont: TFont;
        fCaptionPos: TGroupBoxCaptionPos;
        procedure Changed;
        procedure CaptionFontChanged (Sender: TObject);
        procedure SetShowBorder (Value: Boolean);
        procedure SetCaptionPos (Value: TGroupBoxCaptionPos);
        procedure SetCaptionFont (Value: TFont);
        procedure SetCaptionSpaces (Value: Boolean);
    public
        constructor Create;
        destructor Destroy; override;
    published
        property OnChange: TNotifyEvent read fOnChange write fOnChange;
        property CaptionSpaces: Boolean read fCaptionSpaces write SetCaptionSpaces default True;
        property ShowBorder: Boolean read fShowBorder write SetShowBorder default True;
        property CaptionPos: TGroupBoxCaptionPos read fCaptionPos write SetCaptionPos default gbTopLeft;
        property CaptionFont: TFont read fCaptionFont write SetCaptionFont;
    end;

    TCustomGroupBoxEx = class (TCustomGroupBox)
    private
        fOptions: TGroupBoxOptions;
        fOnEnableDisableQuery: TGroupBoxEnableDisableQuery;
        fOnGroupBoxPaintCaptionBackground: TGroupBoxPaintCaptionBackground;
        procedure OptionsChanged (Sender: TObject);
        procedure CMEnabledChanged (var Msg: TMessage); message cm_EnabledChanged;
    protected
        procedure AdjustClientRect (var Rect: TRect); override;
        procedure Paint; override;
        procedure PaintCaption (Str: String);
    public
        constructor Create (AOwner: TComponent); override;
        destructor Destroy; override;
        property Advanced: TGroupBoxOptions read fOptions write fOptions;
        property OnEnableDisableQuery: TGroupBoxEnableDisableQuery read fOnEnableDisableQuery write fOnEnableDisableQuery;
        property OnPaintCaptionBackground: TGroupBoxPaintCaptionBackground read fOnGroupBoxPaintCaptionBackground write fOnGroupBoxPaintCaptionBackground;
    end;

    TGroupBoxEx = class (TCustomGroupBoxEx)
    published
        property Align;
        property Anchors;
        property BiDiMode;
        property Caption;
        property Color;
        property Constraints;
        property Ctl3D;
        property DockSite;
        property DragCursor;
        property DragKind;
        property DragMode;
        property Enabled;
        property Font;
        property ParentBiDiMode;
        property ParentColor;
        property ParentCtl3D;
        property ParentFont;
        property ParentShowHint;
        property PopupMenu;
        property ShowHint;
        property TabOrder;
        property TabStop;
        property Visible;
        property OnClick;
        property OnContextPopup;
        property OnDblClick;
        property OnDragDrop;
        property OnDockDrop;
        property OnDockOver;
        property OnDragOver;
        property OnEndDock;
        property OnEndDrag;
        property OnEnter;
        property OnExit;
        property OnGetSiteInfo;
        property OnMouseDown;
        property OnMouseMove;
        property OnMouseUp;
        property OnStartDock;
        property OnStartDrag;
        property OnUnDock;
        property Advanced;
        property OnEnableDisableQuery;
        property OnPaintCaptionBackground;
    end;

implementation

{ TGroupBoxOptions }

uses ComCtrls;

constructor TGroupBoxOptions.Create;
begin
    Inherited Create;
    fCaptionPos := gbTopLeft;
    fCaptionSpaces := True;
    fCaptionFont := TFont.Create;
    fCaptionFont.OnChange := CaptionFontChanged;
    fShowBorder := True;
end;

destructor TGroupBoxOptions.Destroy;
begin
    fCaptionFont.Destroy;
    Inherited Destroy;
end;

procedure TGroupBoxOptions.Changed;
begin
    if Assigned (fOnChange) then fOnChange (Self);
end;

procedure TGroupBoxOptions.CaptionFontChanged (Sender: TObject);
begin
    Changed;
end;

procedure TGroupBoxOptions.SetCaptionSpaces (Value: Boolean);
begin
    if fCaptionSpaces <> Value then begin
        fCaptionSpaces := Value;
        Changed;
    end;
end;

procedure TGroupBoxOptions.SetCaptionFont (Value: TFont);
begin
    fCaptionFont.Assign (Value);
    Changed;
end;

procedure TGroupBoxOptions.SetCaptionPos (Value: TGroupBoxCaptionPos);
begin
    if fCaptionPos <> Value then begin
        fCaptionPos := Value;
        Changed;
    end;
end;

procedure TGroupBoxOptions.SetShowBorder (Value: Boolean);
begin
    if fShowBorder <> Value then begin
        fShowBorder := Value;
        Changed;
    end;
end;

{ TCustomGroupBoxEx }

constructor TCustomGroupBoxEx.Create(AOwner: TComponent);
begin
    Inherited Create (AOwner);
    fOptions := TGroupBoxOptions.Create;
    fOptions.OnChange := OptionsChanged;
end;

destructor TCustomGroupBoxEx.Destroy;
begin
    fOptions.Free;
    Inherited Destroy;
end;

procedure TCustomGroupBoxEx.OptionsChanged (Sender: TObject);
begin
    Invalidate;
end;

procedure TCustomGroupBoxEx.AdjustClientRect (var Rect: TRect);
begin
    // Don't pass this on to Inherited.....
end;

procedure TCustomGroupBoxEx.CMEnabledChanged (var Msg: TMessage);
var
    Idx: Integer;
    Child: TControl;
    Handled: Boolean;
begin
    Inherited;
    Invalidate;
    // Now enable or disable all the contained controls
    for Idx := 0 to ControlCount - 1 do begin
        Child := Controls [Idx];
        // Query application to see if we should do it
        Handled := False;
        if Assigned (fOnEnableDisableQuery) then fOnEnableDisableQuery (Self, Child, Enabled, Handled);
        if not Handled then begin
            Child.Enabled := Enabled;

            if Child.ClassName = 'TEdit' then begin
                if Enabled then TEdit (Child).Color := clWindow
                else TEdit (Child).Color := clBtnFace;
            end;

            if Child.ClassName = 'TListBox' then begin
                if Enabled then TListBox (Child).Color := clWindow
                else TListBox (Child).Color := clBtnFace;
            end;

            if Child.ClassName = 'TComboBox' then begin
                if Enabled then TComboBox (Child).Color := clWindow
                else TComboBox (Child).Color := clBtnFace;
            end;

            if Child.ClassName = 'TMemo' then begin
                if Enabled then TMemo (Child).Color := clWindow
                else TMemo (Child).Color := clBtnFace;
            end;

            if Child.ClassName = 'TDateTimePicker' then begin
                if Enabled then TDateTimePicker (Child).Color := clWindow
                else TDateTimePicker (Child).Color := clBtnFace;
            end;

            // Add your own type-specific preferences here?
        end;
    end;
end;

procedure TCustomGroupBoxEx.Paint;
var
    R: TRect;
    H2: Integer;
begin
    with Canvas do begin
        Font := fOptions.CaptionFont;
        H2 := TextHeight ('0') div 2 - 1;
        case fOptions.fCaptionPos of
            gbTopLeft..gbTopRight:
                R := Rect (0, H2, Width, Height);
            gbBottomLeft..gbBottomRight:
                R := Rect (0, 0, Width, Height - H2);
            gbLeftTop..gbLeftBottom:
                R := Rect (H2, 0, Width, Height);
            gbRightTop..gbRightBottom:
                R := Rect (0, 0, Width - H2, Height);
        end;

        if Ctl3D then begin
            Inc(R.Left);
            Inc(R.Top);
            Brush.Color := clBtnHighlight;
            if fOptions.ShowBorder then FrameRect(R);
            OffsetRect (R, -1, -1);
            Brush.Color := clBtnShadow;
        end else Brush.Color := clWindowFrame;
        if fOptions.ShowBorder then FrameRect(R);

        if Text <> '' then PaintCaption (Text);
    end;
end;

procedure TCustomGroupBoxEx.PaintCaption (Str: String);
var
    R: TRect;
    lf: TLogFont;
    tm: TTextMetric;
    BackgroundHandled: Boolean;        
    X, Y, Flags, TH, TW: Integer;
begin
    if fOptions.CaptionSpaces then Str := ' ' + Str + ' ';
    if fOptions.CaptionPos in [gbLeftTop, gbRightBottom] then Str := Str + ' ';

    BackgroundHandled := False;
    TH := Canvas.TextHeight (Str);
    TW := Canvas.TextWidth (Str);

    // Deal with the easy stuff first !
    if fOptions.CaptionPos in [gbTopLeft..gbBottomRight] then begin
        R := Rect (8, 0, Width - 16, TH);
        if fOptions.CaptionPos in [gbBottomLeft..gbBottomRight] then OffsetRect (R, 0, Height - TH);

        Flags := dt_SingleLine;
        case fOptions.CaptionPos of
            gbTopLeft, gbBottomLeft:     Flags := Flags or dt_Left;
            gbTopMiddle, gbBottomMiddle: Flags := Flags or dt_Center;
            gbTopRight, gbBottomRight:   Flags := Flags or dt_Right;
        end;

        if Assigned (OnPaintCaptionBackground) then
            OnPaintCaptionBackground (Self, Canvas, R, BackgroundHandled);

        Canvas.Brush.Color := Color;
        if BackgroundHandled then SetBkMode (Canvas.Handle, Transparent);
        if Enabled then DrawText (Canvas.Handle, PChar (Str), -1, R, Flags) else begin
            SetTextColor (Canvas.Handle, ColorToRGB (clBtnHighlight));
            DrawText (Canvas.Handle, PChar (Str), -1, R, Flags);
            OffsetRect (R, -1, -1);
            SetBkMode (Canvas.Handle, Transparent);
            SetTextColor (Canvas.Handle, ColorToRGB (clBtnShadow));
            DrawText (Canvas.Handle, PChar (Str), -1, R, Flags);
        end;
    end else begin
        R := Rect (0, 8, TH, Height - 16);
        if fOptions.CaptionPos in [gbRightTop..gbRightBottom] then OffsetRect (R, Width - TH, 0);

        // This is only going to work with TrueType fonts....
        GetTextMetrics (Canvas.Handle, tm);
        if (tm.tmPitchAndFamily and tmpf_TrueType) = 0 then Exit;

        if Assigned (OnPaintCaptionBackground) then
            OnPaintCaptionBackground (Self, Canvas, R, BackgroundHandled);

        // Now build a new, vertical font.....
        GetObject (Canvas.Font.Handle, sizeOf (lf), @lf);
        if fOptions.CaptionPos in [gbLeftTop..gbLeftBottom] then lf.lfEscapement := 900
        else lf.lfEscapement := 2700;
        Canvas.Font.Handle := CreateFontIndirect (lf);

        Canvas.Brush.Color := Color;
        X := R.Left;  Y := R.Top;
        case fOptions.CaptionPos of
            gbLeftTop,    gbRightTop:     Y := 8 + TW;
            gbLeftMiddle, gbRightMiddle:  Y := ((Height - TW) div 2) + TW;
            gbLeftBottom, gbRightBottom:  Y := Height - 16;
        end;

        if lf.lfEscapement = 2700 then begin
            Dec (Y, TW);  Inc (X, TH);
        end;

        if BackgroundHandled then SetBkMode (Canvas.Handle, Transparent);
        if Enabled then ExtTextOut (Canvas.Handle, X, Y, 0, Nil, PChar (Str), Length (Str), Nil) else begin
            SetTextColor (Canvas.Handle, ColorToRGB (clBtnHighlight));
            ExtTextOut (Canvas.Handle, X, Y, 0, Nil, PChar (Str), Length (Str), Nil);
            Dec (X);  Dec (Y);
            SetBkMode (Canvas.Handle, Transparent);
            SetTextColor (Canvas.Handle, ColorToRGB (clBtnShadow));
            ExtTextOut (Canvas.Handle, X, Y, 0, Nil, PChar (Str), Length (Str), Nil);
        end;
    end;
end;

end.
