inherited fraSpecimenFinderDragFrame: TfraSpecimenFinderDragFrame
  Height = 318
  object pnlDataSelection: TPanel
    Left = 0
    Top = 235
    Width = 371
    Height = 83
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object rgDataSelectionMode: TRadioGroup
      Left = 0
      Top = 0
      Width = 371
      Height = 83
      Align = alClient
      Caption = 'Data Selection Mode:'
      ItemIndex = 0
      Items.Strings = (
        
          'Find Specimens that are associated with a&ll of the above criter' +
          'ia (logical AND)'
        
          'Find Specimens that are associated with a&ny of the above criter' +
          'ia (logical OR)')
      TabOrder = 0
      OnClick = rgDataSelectionModeClick
    end
  end
  object pnlLabelSpacer: TPanel
    Left = 0
    Top = 0
    Width = 371
    Height = 21
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object Label1: TLabel
      Left = 4
      Top = 4
      Width = 151
      Height = 13
      Caption = 'Drag items you are interested in:'
    end
  end
  object pnlFinder: TPanel
    Left = 0
    Top = 21
    Width = 371
    Height = 214
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      371
      214)
    object shpFinder: TShape
      Left = 1
      Top = 2
      Width = 368
      Height = 210
      Anchors = [akLeft, akTop, akRight, akBottom]
      Pen.Color = clRed
    end
    object lbFinder: TListBox
      Left = 2
      Top = 3
      Width = 366
      Height = 208
      Style = lbOwnerDrawFixed
      Anchors = [akLeft, akTop, akRight, akBottom]
      ItemHeight = 16
      TabOrder = 0
      OnDrawItem = lbFinderDrawItem
      OnKeyDown = lbFinderKeyDown
    end
  end
  object ilSpecimenFinder: TImageList
    Left = 32
    Top = 172
    Bitmap = {
      494C010109000E00040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000004000000001001000000000000020
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      1000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000001000
      1000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000010003C25
      1000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000001000DD39BD35
      1000100010001000100010001000100000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000010005E4A3D46FD3D
      FD3DDD397D2D7D2D3C253C25FC1C100000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000001000FE5EDE5ABE563D46
      FD3D0058DD397D2D7D2D3C253C25100000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000001000FE5E1E63BE56
      3D4600580058DD397D2D7D2D3C25100000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000001000FE5E1E63
      10000058C6700058100010001000100000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000580058005800581000FE5E
      100000580871E770005800000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000005810761076107610761000
      10008C756B752971E77000580000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000058737A737A737A317A1076
      1000CE75AD756B752971E7700058000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000058D67AD67AD67AB57A737A
      737A1076CE75AD756B7500580000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000005800580058005800580058
      005800581076CE75005800000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000058317A0058000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000005800580000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000005800000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000524A734E104208214A29
      0000000000000000000000000000000000000000000000000000000000001300
      13000C000C000000000000000000000000000000000000000000000029252925
      29252925000000000000000000000000000000002C67604E604E000000000000
      000000000000867D867D00000000000000000000524A8C3100000000D65A1042
      0821000000000000000000000000000000000000000000000000000000001300
      8C318C310C0000000000000000000000000000000000000000002925CF08EE14
      2E1D2925292529252925000000000000000000002C672C67604E604E604E0000
      0000867D867D867D6C7E867D337F337F00000000524A1042D65A9C7339671863
      1042082100000000000000000000000000000000000000000000000000001900
      734E8C31130000000000130013000C000C0000000000000029257701F304193A
      5719F304AF08EE142D21292529250000000000002C67604E604E2C672C67604E
      604E0000867D6C7E867D6C7E337F337F00000000524A8C31524A29251042D65A
      D65ACE3942080000000000000000000000000000000000000000000000001900
      190013001300000000001300C618C6180C00000000002925BA019901961D7D6B
      3C5F1C5BDB52593E1615121529250000000000002C672C67604E000000002C67
      604E337F337F867D6C7E867D6C7E867D00000000524A94529C7394521042524A
      945210424208E71CE71CE71C0000000000001F00D900D900D900000000000000
      0000000000000000000013008C31C618130000002925FB01FB01B901B6219E6F
      5C631C5FDB52BA4E9946794629250000000000002C67604E2C67604E604E0000
      2C67337F337F0000867D6C7E867D867D00000000524A1042D65A10422925E71C
      CE39CE39C618082108210821E71C000000001F00FF7F3967D900000000001900
      1900190013000000000013001300130013000000D91D3D021C02BA01B625BE73
      7D6B954E764A79429A4A994A292500000000000000002C672C672C67604E604E
      0000337F0000604E0000867D867D867D000000001042CE399C737B6F10428C31
      4A2910421042104210424A29082108210000DF00FF7FFF7FD90000000000D900
      734E734E13000000000000000000000000000000F91D3E021D02DB012F19BE73
      9E6F935EB65ACB4DDB52BA4E29250000000000000000000000002C672C67604E
      604E0000604E2C672C67000000000000000000004A297B6F3967D65A10421042
      1042734E9452734E104210424A2908210000DF00DF001F001F0000000000D900
      3967734E19000000000019001300130013000000F91D5E023E02DB014604BE77
      BE737D6B5C631C5BDB52BA52292500000000000000000000000000002C672C67
      2C67604E2C670000000000000000000000000000C6184A290821D65A524A524A
      D65AD65AD65A734E1042104210424A298C31000000000000000000000000D900
      D900D9001900000000001900734E8C3113000000FA1D5E023E02FB019A3EDC46
      BB3EFC4E3E53D952FB52DA56292500000000000000000000000000002C67337F
      337F337F00000000000000000000000000000000E71C104200003967D65A734E
      945218631863D65A734E734E104208218C310000000000000000000000000000
      00000000000000000000D900734E734E13000000FE1D7F025E02FF36FF7F5D63
      5D5F3E571732F00CCE0853212925000000000000000000000000004C004C337F
      6C666C6600000000000000000000000000000000524AD65ABD779C737B6FD65A
      94521863186318639452734E104208218C31000000000000000000000000D900
      D900D900D90000000000D900D9001900190000001E1E7F0A7F023E02BE73BE73
      5D63BA4A79429A4639360F1100000000000000000000000000000064004C337F
      337F337F6C66000000000000000000000000000000001042FF7FFF7F9C737B6F
      5A6B18631863D65A734E734E104208218C310000000000000000000000001F00
      39673967D90000000000000000000000000000003D269F127F063E023C5FFC4A
      5A3A79429946BA4E793A101100000000000000000000000000000064004C004C
      337F0000337F00000000000000000000000000000000524A7B6FFF7FFF7FFF7F
      9C739C733967D65A945294521042CE390821000000000000000000000000DF00
      FF7F3967D90000000000D900D900D900D90000003D2EBF1A9F125E025D639E6F
      316F6E56E955DB529A3E3011000000000000000000000000000000640064004C
      004C337F00000000000000000000000000000000000000004A29D65A9C73FF7F
      9C739C733967D65A94528C3110428C310000000000000000000000000000DF00
      DF001F001F00000000001F003967734ED90000003D32DF26BF167F0A7E63BE73
      7D6B5C631C5BFB52BB4231110000000000000000000000000000006400640064
      0064004C004C0000000000000000000000000000000000000000524A8C318C31
      8C318C318C318C318C318C31C618000000000000000000000000000000000000
      000000000000000000001F0039673967D90000005D36FF2EDF229F127F577E63
      5E5F3D571D53FC4ABC3A97150000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000DF001F001F00D90000005C3A5C3A5C3ABB52BB5A1C67
      BB5ABB429A3E5C3A39360000000000000000000000000000000000000000084E
      A120073100000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000930893089308
      9308930893089308000000000000000000000000000000000000AF62C54D225E
      4045885147414639000000000000000000000000AD69AD696B594A5108410000
      8D6A8D6A2B5AEA51884100000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000093089C367B32
      9308F925781D93080000000000000000000000000000AF620B4AC54D407E407E
      E044A55D0962C851693946390000000000000000AD69527A107210726B590841
      8D6A127BD072D0722B5A884100000000000000008C016C1A6C1A6C1A00008001
      600260026002600260026002600200000000000000000000000093087B325B2E
      9308F925F925930800000000000000000000AF62644981594376407E407E407E
      4139865909620962096224494439463900000000CE69CE518C516B4929496B59
      8E6A2E520C52CB49A9494E5A8841000000000000CC008C016C1A6C1A6C1A0000
      800160026002600260026002600200000000000000000000000093087B325B2E
      5B2EF925F9259308000000000000000000008520804D407E407E407E407E407E
      6035A7590962096209620962C94D893D00000000EF69947A947A527AAD696B59
      8E6A347B347B127B8D6A084188410000000000007F1ACC008C016C1A6C1A7332
      0000800160026002600260022C0300000000000000000000B20893087B327B32
      5B2EF925F9259308930800000000000000004120407E407E407E407E407E407E
      203185510962096209620962096225350000000010721062CE618C596B516B59
      D07290626E622C5AEB514E5A88410000000000007F1A7F1ACC008C017332334F
      7F1A00008001600260022C03EC4F00000000000000009308DC3A9C3693087B32
      5B2E5B2E9308F925F9259308000000000000A22C407E407E407E407E407E407E
      203988518655233D0962096209620439000000001072D67ED67E947ACE696B59
      D072767F767F347B8E6AAB3D88410000000000007F1A7F1A7F1A7332334F7F1A
      7F1A7F1A00002C032C03EC4F7F1A00000000000000009308DC3ADC3693087B32
      7B325B2E9308F925F92593080000000000008124407E407E407E407E407E614D
      A1202241E061846E6151C24409622535000000003172F77ED67EB57ACE696B59
      F172777F767F357B8E6A4E5A88410000000000007F1A7F1A7F1A7F1A7F1A7F1A
      7F1A7F1A7F1AEC4FEC4F7F1A7F1A00000000000000009308DC36DC3ADC369C36
      7B327B325B2E5B2EF9259308000000000000822C407E407E407E407EC9558C31
      6B2D8A39E044C05DA07A407644418120000000005272F77EF77ED67ECE696B59
      F272777F777F767F8E6A4E5A88410000000000007F1A3F337F1A3F337F1A3F33
      7F1A3F337F1A3F337F1A3F337F1A000000000000000000009308DC36DC36DC36
      9C367B327B325B2E93080000000000000000802C407E617EE35D2435EC513146
      9452B556272DE240C3556266A17E216E40450000947AF77EF77EF77ECE696B59
      347B777F777F777F8E6A4E5A88410000000000003F337F1A3F337F1A3F337F1A
      3F337F1A3F332067E67F7F1A3F3300000000000000000000000093089308DC36
      DC369C369308930800000000000000000000E22CA1594051003DC130EC51D65A
      EF3DEF3D4A29E71C272DE03C8055E5592B560000B57A106AEF61CE618C596B59
      357BB06A6F626E622C5A4E5A88410000000000003F333F333F333F333F333F33
      3F333F332067E67FE67FE67F3F33000000000000000000000000000000009308
      DC36930800000000000000000000000000006A318124E1386345233DAA491863
      9452AD35314631466B2D473D8024AA4116670000B57AF77EF77EF77EEF696B59
      357B777F777F777F8E6AAB3D88410000000000003F333F673F333F673F333F67
      3F333F67E67FE67FE67FE67F3F3300000000000000000000000000009308FD3A
      DC3ADC36930800000000000000000000000000005352AD452535233946390821
      D65A9452F75E734E31464A29955A000000000000B57AB57A947A3172CE696B59
      357B357B347BF1728E6A075A88410000000000003F673F333F673F333F673F33
      3F673F333F67E67FE67F3F333F6700000000000000000000000093081D3FFD3E
      FD3ADC3ADC3A93080000000000000000000000000000000093566739C9551863
      9C7318631863B55631464A2900000000000000000000736ACE499C73FE7F526A
      0841B262503A9C73FE7F0E5288410000000000003F673F673F673F673F673F67
      3F673F673F673F673F673F673F6700000000000000000000000093081D3F1D3F
      FD3EFD3AFD3A93080000000000000000000000000000000000000000735A2F4E
      9C73BD777B6FB55610428A390000000000000000000000005262CE495A735A73
      526A0000925A10425A735A738841000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000093089308
      9308930893080000000000000000000000000000000000000000000000000000
      CB4D894589458945683D00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000400000000100010000000000000200000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000FEFF000000000000FCFF000000000000
      F8FF000000000000F001000000000000E001000000000000C001000000000000
      E001000000000000F001000000000000800F0000000000008007000000000000
      80030000000000008007000000000000800F000000000000FF1F000000000000
      FF3F000000000000FF7F0000000000008FCFC1FFFC3FF87F030180FFFC3FF00F
      0000807FF830E0030000803FF430C0030000800707E080030000800304108003
      80008001001F8003C000800104308003F00F8000F4308003F03F8000FBE08003
      E03F8000F8208007E01FC000FC1F8007E01FC000FC108007E03FE001FC208007
      E01FF003FFF08007F03FFFFFFFF0800FFC7FFFFFFFFFF01FF01F820F0001F01F
      C00780070001F01F000180030001F01F000180030001E00F000180030001C007
      000180030001C007000180030001C007000180030001E00F000080030001F01F
      000080030001FC7F000080030001F83F800380030001F01FE007C0030001F01F
      F807E0830001F83FFE0FFFFFFFFFFFFF00000000000000000000000000000000
      000000000000}
  end
end