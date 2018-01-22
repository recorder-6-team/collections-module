inherited fraConceptGeneral: TfraConceptGeneral
  Font.Name = 'Lucida Sans Unicode'
  object Label2: TLabel
    Left = 12
    Top = 116
    Width = 29
    Height = 15
    Caption = '&Rank:'
    FocusControl = cmbRank
  end
  object Label1: TLabel
    Left = 12
    Top = 172
    Width = 63
    Height = 15
    Caption = 'Name Type:'
  end
  object Label3: TLabel
    Left = 12
    Top = 204
    Width = 53
    Height = 15
    Caption = 'List Code:'
  end
  object Label4: TLabel
    Left = 12
    Top = 236
    Width = 56
    Height = 15
    Caption = 'Sort Code:'
  end
  object lblTerm: TLabel
    Left = 12
    Top = 20
    Width = 91
    Height = 15
    Caption = 'Formatted Term:'
  end
  object lblLanguage: TLabel
    Left = 12
    Top = 84
    Width = 55
    Height = 15
    Caption = 'Language:'
  end
  object Label5: TLabel
    Left = 12
    Top = 46
    Width = 72
    Height = 30
    Caption = 'Unformatted Term:'
    WordWrap = True
  end
  object lblUnformatted: TLabel
    Left = 104
    Top = 52
    Width = 69
    Height = 15
    Caption = 'Lasius flavus'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Lucida Sans Unicode'
    Font.Style = []
    ParentFont = False
  end
  object cmbRank: TComboBox
    Left = 104
    Top = 112
    Width = 193
    Height = 23
    Style = csDropDownList
    ItemHeight = 15
    TabOrder = 0
    Items.Strings = (
      'Class'
      'Phylum'
      'Family'
      'Genus'
      'Species')
  end
  object cmbNameType: TComboBox
    Left = 104
    Top = 168
    Width = 193
    Height = 23
    Style = csDropDownList
    ItemHeight = 15
    TabOrder = 1
    Items.Strings = (
      'Common'
      'Formal'
      'Common typographic error')
  end
  object chkPreferred: TCheckBox
    Left = 12
    Top = 144
    Width = 105
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Preferred:'
    TabOrder = 2
  end
  object eListCode: TEdit
    Left = 104
    Top = 200
    Width = 193
    Height = 23
    TabOrder = 3
  end
  object eSortCode: TEdit
    Left = 104
    Top = 232
    Width = 193
    Height = 23
    TabOrder = 4
  end
  object cmbLanguage: TComboBox
    Left = 104
    Top = 80
    Width = 193
    Height = 23
    Style = csDropDownList
    ItemHeight = 15
    Sorted = True
    TabOrder = 5
    Items.Strings = (
      'a - Hausa'
      'aa - Afar'
      'ab - Abkhazian'
      'af - Afrikaans'
      'am - Amharic'
      'ar - Arabic'
      'as - Assamese'
      'ay - Aymara'
      'az - Azerbaijani'
      'ba - Bashkir'
      'be - Byelorussian'
      'bg - Bulgarian'
      'bh - Bihari'
      'bi - Bislama'
      'bn - Bengali;Bangla'
      'bo - Tibetan'
      'br - Breton'
      'ca - Catalan'
      'co - Corsican'
      'cs - Czech'
      'cy - Welsh'
      'cy - Welsh'
      'da - Danish'
      'de - German'
      'dz - Bhutani'
      'el - Greek'
      'en - English'
      'eo - Esperanto'
      'es - Spanish'
      'es - Spanish'
      'et - Estonian'
      'eu - Basque'
      'fa - Persian (Farsi)'
      'fi - Finnish'
      'fj -Fiji'
      'fo - Faroese'
      'fr - French'
      'fy - Frisian'
      'ga - Gaelic (Irish)'
      'gd - Gaelic (Scottish)'
      'gl - Galician'
      'gnv - Guarani'
      'he - Hebrew'
      'hi - Hindi'
      'hr - Croatian'
      'hu - Hungarian'
      'hu - Hungarian'
      'hy - Armenian'
      'ia - Interlingua'
      'id - Indonesian'
      'ie - Interlingue'
      'ikv - Inupiak'
      'is - Icelandic'
      'it - Italian'
      'it - Italian'
      'iu - Inuktitut'
      'ja - Japanese'
      'jv - Javanese'
      'ka - Georgian'
      'kk - Kazakh'
      'klv - Greenlandic'
      'km - Cambodian'
      'kn - Kannada'
      'ko - Korean'
      'ks - Kashmiri'
      'ku - Kurdish'
      'ky - Kirghiz'
      'la - Latin'
      'ln - Lingala'
      'lo - Laothian'
      'lt - Lithuanian'
      'lt - Lithuanian'
      'lv - Latvian;Lettish '
      'mg - Malagasy'
      'mi - Maori'
      'mk - Macedonian'
      'mk - Macedonian'
      'ml - Malayalam'
      'mn - Mongolian'
      'mo - Moldavian'
      'mr - Marathi'
      'ms - Malay'
      'mt - Maltese'
      'my - Burmese'
      'na - Nauru'
      'ne - Nepali'
      'nl - Dutch'
      'no - Norwegian'
      'no - Norwegian'
      'oc - Occitan'
      'om - Afan (Oromo)'
      'or - Oriya'
      'pa - Punjabi'
      'pl - Polish'
      'pl - Polish'
      'ps - Pashto;Pushto'
      'pt - Portuguese'
      'pt - Portuguese'
      'qu - Quechua'
      'rm - Rhaeto-Romance'
      'rn - Kurundi'
      'ro - Romanian'
      'ro - Romanian'
      'ru - Russian'
      'rw - Kinyarwanda'
      'sa - Sanskrit'
      'sd - Sindhi'
      'sg - Sangho'
      'sh - Serbo-Croatian'
      'sh - Serbo-Croatian'
      'si - Singhalese'
      'sk - Slovak'
      'sk - Slovak'
      'sl - Slovenian'
      'sl - Slovenian'
      'sm - Samoan'
      'sn - Shona'
      'so - Somali'
      'sq - Albanian'
      'sr - Serbian'
      'sr - Serbian'
      'ss - Siswati'
      'st - Sesotho'
      'su - Sundanese'
      'sv - Swedish'
      'sv - Swedish'
      'sw - Swahili'
      'ta - Tamil'
      'te - Telugu'
      'tg - Tajik'
      'th - Thai'
      'ti - Tigrinya'
      'tk - Turkmen'
      'tl - Tagalog'
      'tn - Setswana'
      'to - Tonga'
      'tr - Turkish'
      'ts - Tsonga'
      'tt - Tatar'
      'tw - Twi'
      'ug - Uigur'
      'uk - Ukrainian'
      'ur - Urdu'
      'uz - Uzbek'
      'vi - Vietnamese'
      'vo - Volapuk'
      'vu - Gujarati'
      'wo - Wolof'
      'xh - Xhosa'
      'yi - Yiddish'
      'yo - Yoruba'
      'za - Zhuang'
      'zh - Chinese'
      'zu - Zulu')
  end
  object reTerm: TRichEdit
    Left = 104
    Top = 16
    Width = 169
    Height = 22
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 6
    WantReturns = False
    WordWrap = False
  end
  object btnItalic: TImageListButton
    Left = 300
    Top = 16
    Width = 22
    Height = 22
    Anchors = [akTop, akRight]
    TabOrder = 7
    ImageList = dmInterface.ilButtons
    ImageIndex = 8
  end
  object btnInsertBotanical: TImageListButton
    Left = 324
    Top = 16
    Width = 22
    Height = 22
    Anchors = [akTop, akRight]
    TabOrder = 8
    OnClick = btnInsertBotanicalClick
    ImageList = dmInterface.ilButtons
    ImageIndex = 9
  end
  object btnTermFind: TImageListButton
    Left = 276
    Top = 16
    Width = 22
    Height = 22
    Anchors = [akTop, akRight]
    TabOrder = 9
    ImageList = dmInterface.ilButtons
    ImageIndex = 12
  end
  object pmBotanicalNames: TPopupMenu
    Left = 320
    Top = 44
    object pmTermBotanicalNames: TMenuItem
      Caption = 'Botanical name construct list'
    end
  end
end
