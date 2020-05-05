function GetUnicodeBidiClassEx(u: LongWord): TUnicodeBidiClass;
begin //generated 2020-05-05
  case u of
  $00000..$07FFF:
    case u of
    $00000..$003FF:
      case u of
        $00..$08, $0E..$1B, $7F..$84, $86..$9F, $AD: result := ubcBoundaryNeutral;
        $09, $0B, $1F: result := ubcSegmentSeparator;
        $0A, $0D, $1C..$1E, $85: result := ubcParagraphSeparator;
        $0C, $20: result := ubcWhiteSpace;
        $41..$5A, $61..$7A, $AA, $B5, $BA, $C0..$D6, $D8..$F6, $F8..$2B8, $2BB..$2C1, $2D0, $2D1, 
        $2E0..$2E4, $2EE, $370..$373, $376, $377, $37A..$37D, $37F, $386, $388..$38A, $38C, $38E..$3A1, 
        $3A3..$3F5, $3F7: result := ubcLeftToRight;
        $3F8..$3FF: result := ubcLeftToRight;
        $30..$39, $B2, $B3, $B9: result := ubcEuropeanNumber;
        $2B, $2D: result := ubcEuropeanNumberSeparator;
        $23..$25, $A2..$A5, $B0, $B1: result := ubcEuropeanNumberTerminator;
        $2C, $2E, $2F, $3A, $A0: result := ubcCommonSeparator;
        $300..$36F: result := ubcNonSpacingMark;
        $21, $22, $26, $27, $2A, $3B, $3D, $3F, $40, $5C, $5E..$60, $7C, $7E, $A1, $A6..$A9, $AC, 
        $AE, $AF, $B4, $B6..$B8, $BC..$BF, $D7, $F7, $2B9, $2BA, $2C2..$2CF, $2D2: result := ubcOtherNeutrals;
        $2D3..$2DF, $2E5..$2ED, $2EF..$2FF, $374, $375, $37E, $384, $385, $387, $3F6: result := ubcOtherNeutrals;
        $28, $29, $3C, $3E, $5B, $5D, $7B, $7D, $AB, $BB: result := ubcMirroredNeutral;
      else result := ubcUnknown;
      end;
    $00400..$007FF:
      case u of
        $400..$482, $48A..$52F, $531..$556, $559..$589: result := ubcLeftToRight;
        $5BE, $5C0, $5C3, $5C6, $5D0..$5EA, $5EF..$5F4, $7C0..$7EA, $7F4, $7F5, $7FA, $7FE, $7FF: result := ubcRightToLeft;
        $608, $60B, $60D, $61B, $61C, $61E..$64A, $66D..$66F, $671..$6D5, $6E5, $6E6, $6EE, $6EF, 
        $6FA..$70D, $70F, $710, $712..$72F, $74D..$7A5, $7B1: result := ubcArabicLetter;
        $6F0..$6F9: result := ubcEuropeanNumber;
        $58F, $609, $60A, $66A: result := ubcEuropeanNumberTerminator;
        $600..$605, $660..$669, $66B, $66C, $6DD: result := ubcArabicNumber;
        $60C: result := ubcCommonSeparator;
        $483..$489, $591..$5BD, $5BF, $5C1, $5C2, $5C4, $5C5, $5C7, $610..$61A, $64B..$65F, $670, 
        $6D6..$6DC, $6DF..$6E4, $6E7, $6E8, $6EA..$6ED, $711, $730..$74A, $7A6..$7B0, $7EB..$7F3, 
        $7FD: result := ubcNonSpacingMark;
        $58A, $58D, $58E, $606, $607, $60E, $60F, $6DE, $6E9, $7F6..$7F9: result := ubcOtherNeutrals;
      else result := ubcUnknown;
      end;
    $00800..$00BFF:
      case u of
        $904..$939, $93D, $950, $958..$961, $964..$980, $985..$98C, $98F, $990, $993..$9A8, $9AA..$9B0, 
        $9B2, $9B6..$9B9, $9BD, $9CE, $9DC, $9DD, $9DF..$9E1, $9E6..$9F1, $9F4..$9FA, $9FC, $9FD, 
        $A05..$A0A, $A0F, $A10, $A13..$A28, $A2A: result := ubcLeftToRight;
        $A2B..$A30, $A32, $A33, $A35, $A36, $A38, $A39, $A59..$A5C, $A5E, $A66..$A6F, $A72..$A74, 
        $A76, $A85..$A8D, $A8F..$A91, $A93..$AA8, $AAA..$AB0, $AB2, $AB3, $AB5..$AB9, $ABD, $AD0, 
        $AE0, $AE1, $AE6..$AF0, $AF9, $B05..$B0C, $B0F: result := ubcLeftToRight;
        $903, $93B, $93E..$940, $949..$94C, $94E, $94F, $982, $983, $9BE..$9C0, $9C7, $9C8, $9CB, 
        $9CC, $9D7, $A03, $A3E..$A40, $A83, $ABE..$AC0, $AC9, $ACB, $ACC, $B02, $B03, $B3E, $B40, 
        $B47, $B48, $B4B, $B4C, $B57: result := ubcCombiningLeftToRight;
        $B10, $B13..$B28, $B2A..$B30, $B32, $B33, $B35..$B39, $B3D, $B5C, $B5D, $B5F..$B61, $B66..$B77, 
        $B83, $B85..$B8A, $B8E..$B90, $B92..$B95, $B99, $B9A, $B9C, $B9E, $B9F, $BA3, $BA4, $BA8..$BAA, 
        $BAE..$BB9, $BD0, $BE6..$BF2: result := ubcLeftToRight;
        $800..$815, $81A, $824, $828, $830..$83E, $840..$858, $85E: result := ubcRightToLeft;
        $860..$86A, $8A0..$8B4, $8B6..$8C7: result := ubcArabicLetter;
        $BBE, $BBF, $BC1, $BC2, $BC6..$BC8, $BCA..$BCC, $BD7: result := ubcCombiningLeftToRight;
        $9F2, $9F3, $9FB, $AF1, $BF9: result := ubcEuropeanNumberTerminator;
        $8E2: result := ubcArabicNumber;
        $816..$819, $81B..$823, $825..$827, $829..$82D, $859..$85B, $8D3..$8E1, $8E3..$902, $93A, 
        $93C, $941..$948, $94D, $951..$957, $962, $963, $981, $9BC, $9C1..$9C4, $9CD, $9E2, $9E3, 
        $9FE, $A01, $A02, $A3C, $A41: result := ubcNonSpacingMark;
        $A42, $A47, $A48, $A4B..$A4D, $A51, $A70, $A71, $A75, $A81, $A82, $ABC, $AC1..$AC5, $AC7, 
        $AC8, $ACD, $AE2, $AE3, $AFA..$AFF, $B01, $B3C, $B3F, $B41..$B44, $B4D, $B55, $B56, $B62, 
        $B63, $B82, $BC0: result := ubcNonSpacingMark;
        $BCD: result := ubcNonSpacingMark;
        $BF3..$BF8, $BFA: result := ubcOtherNeutrals;
      else result := ubcUnknown;
      end;
    $00C00..$00FFF:
      case u of
        $C05..$C0C, $C0E..$C10, $C12..$C28, $C2A..$C39, $C3D, $C58..$C5A, $C60, $C61, $C66..$C6F, 
        $C77, $C7F, $C80, $C84..$C8C, $C8E..$C90, $C92..$CA8, $CAA..$CB3, $CB5..$CB9, $CBD, $CBF, 
        $CC6, $CDE, $CE0, $CE1, $CE6..$CEF, $CF1: result := ubcLeftToRight;
        $CF2, $D04..$D0C, $D0E..$D10, $D12..$D3A, $D3D, $D4E, $D4F, $D54..$D56, $D58..$D61, $D66..$D7F, 
        $D85..$D96, $D9A..$DB1, $DB3..$DBB, $DBD, $DC0..$DC6, $DE6..$DEF, $DF4, $E01..$E30, $E32, 
        $E33, $E40..$E46, $E4F..$E5B, $E81, $E82, $E84: result := ubcLeftToRight;
        $E86..$E8A, $E8C..$EA3, $EA5, $EA7..$EB0, $EB2, $EB3, $EBD, $EC0..$EC4, $EC6, $ED0..$ED9, 
        $EDC..$EDF, $F00..$F17, $F1A..$F34, $F36, $F38, $F40..$F47, $F49..$F6C, $F85, $F88..$F8C, 
        $FBE..$FC5, $FC7..$FCC, $FCE..$FDA: result := ubcLeftToRight;
        $C01..$C03, $C41..$C44, $C82, $C83, $CBE, $CC0..$CC4, $CC7, $CC8, $CCA, $CCB, $CD5, $CD6, 
        $D02, $D03, $D3E..$D40, $D46..$D48, $D4A..$D4C, $D57, $D82, $D83, $DCF..$DD1, $DD8..$DDF, 
        $DF2, $DF3, $F3E, $F3F, $F7F: result := ubcCombiningLeftToRight;
        $E3F: result := ubcEuropeanNumberTerminator;
        $C00, $C04, $C3E..$C40, $C46..$C48, $C4A..$C4D, $C55, $C56, $C62, $C63, $C81, $CBC, $CCC, 
        $CCD, $CE2, $CE3, $D00, $D01, $D3B, $D3C, $D41..$D44, $D4D, $D62, $D63, $D81, $DCA, $DD2..$DD4, 
        $DD6, $E31, $E34: result := ubcNonSpacingMark;
        $E35..$E3A, $E47..$E4E, $EB1, $EB4..$EBC, $EC8..$ECD, $F18, $F19, $F35, $F37, $F39, $F71..$F7E, 
        $F80..$F84, $F86, $F87, $F8D..$F97, $F99..$FBC, $FC6: result := ubcNonSpacingMark;
        $C78..$C7E: result := ubcOtherNeutrals;
        $F3A..$F3D: result := ubcMirroredNeutral;
      else result := ubcUnknown;
      end;
    $01000..$017FF:
      case u of
        $1680: result := ubcWhiteSpace;
        $1000..$102A, $103F..$1055, $105A..$105D, $1061, $1065, $1066, $106E..$1070, $1075..$1081, 
        $108E, $1090..$1099, $109E..$10C5, $10C7, $10CD, $10D0..$1248, $124A..$124D, $1250..$1256, 
        $1258, $125A..$125D, $1260..$1288, $128A..$128D, $1290..$12B0, $12B2..$12B5, $12B8: result := ubcLeftToRight;
        $12B9..$12BE, $12C0, $12C2..$12C5, $12C8..$12D6, $12D8..$1310, $1312..$1315, $1318..$135A, 
        $1360..$137C, $1380..$138F, $13A0..$13F5, $13F8..$13FD, $1401..$167F, $1681..$169A, $16A0..$16F8, 
        $1700..$170C, $170E..$1711, $1720..$1731, $1735, $1736, $1740..$1751, $1760..$176C, $176E..$1770, 
        $1780: result := ubcLeftToRight;
        $1781..$17B3, $17D4..$17DA, $17DC, $17E0..$17E9: result := ubcLeftToRight;
        $102B, $102C, $1031, $1038, $103B, $103C, $1056, $1057, $1062..$1064, $1067..$106D, $1083, 
        $1084, $1087..$108C, $108F, $109A..$109C, $17B6, $17BE..$17C5, $17C7, $17C8: result := ubcCombiningLeftToRight;
        $17DB: result := ubcEuropeanNumberTerminator;
        $102D..$1030, $1032..$1037, $1039, $103A, $103D, $103E, $1058, $1059, $105E..$1060, $1071..$1074, 
        $1082, $1085, $1086, $108D, $109D, $135D..$135F, $1712..$1714, $1732..$1734, $1752, $1753, 
        $1772, $1773, $17B4, $17B5, $17B7..$17BD, $17C6, $17C9..$17D3, $17DD: result := ubcNonSpacingMark;
        $1390..$1399, $1400, $17F0..$17F9: result := ubcOtherNeutrals;
        $169B, $169C: result := ubcMirroredNeutral;
      else result := ubcUnknown;
      end;
    $01800..$01FFF:
      case u of
        $180E: result := ubcBoundaryNeutral;
        $1810..$1819, $1820..$1878, $1880..$1884, $1887..$18A8, $18AA, $18B0..$18F5, $1900..$191E, 
        $1946..$196D, $1970..$1974, $1980..$19AB, $19B0..$19C9, $19D0..$19DA, $1A00..$1A16, $1A1E..$1A54, 
        $1A80..$1A89, $1A90..$1A99, $1AA0..$1AAD, $1B05..$1B33, $1B45..$1B4B, $1B50..$1B6A, $1B74..$1B7C, 
        $1B83: result := ubcLeftToRight;
        $1923..$1926, $1929..$192B, $1930, $1931, $1933..$1938, $1A19, $1A1A, $1A55, $1A57, $1A61, 
        $1A63, $1A64, $1A6D..$1A72, $1B04, $1B35, $1B3B, $1B3D..$1B41, $1B43, $1B44, $1B82, $1BA1, 
        $1BA6, $1BA7, $1BAA, $1BE7, $1BEA..$1BEC, $1BEE: result := ubcCombiningLeftToRight;
        $1B84..$1BA0, $1BAE..$1BE5, $1BFC..$1C23, $1C3B..$1C49, $1C4D..$1C88, $1C90..$1CBA, $1CBD..$1CC7, 
        $1CD3, $1CE9..$1CEC, $1CEE..$1CF3, $1CF5, $1CF6, $1CFA, $1D00..$1DBF, $1E00..$1F15, $1F18..$1F1D, 
        $1F20..$1F45, $1F48..$1F4D, $1F50..$1F57, $1F59, $1F5B, $1F5D, $1F5F: result := ubcLeftToRight;
        $1F60..$1F7D, $1F80..$1FB4, $1FB6..$1FBC, $1FBE, $1FC2..$1FC4, $1FC6..$1FCC, $1FD0..$1FD3, 
        $1FD6..$1FDB, $1FE0..$1FEC, $1FF2..$1FF4, $1FF6..$1FFC: result := ubcLeftToRight;
        $1BF2, $1BF3, $1C24..$1C2B, $1C34, $1C35, $1CE1, $1CF7: result := ubcCombiningLeftToRight;
        $180B..$180D, $1885, $1886, $18A9, $1920..$1922, $1927, $1928, $1932, $1939..$193B, $1A17, 
        $1A18, $1A1B, $1A56, $1A58..$1A5E, $1A60, $1A62, $1A65..$1A6C, $1A73..$1A7C, $1A7F, $1AB0..$1AC0, 
        $1B00..$1B03, $1B34, $1B36..$1B3A, $1B3C, $1B42: result := ubcNonSpacingMark;
        $1B6B..$1B73, $1B80, $1B81, $1BA2..$1BA5, $1BA8, $1BA9, $1BAB..$1BAD, $1BE6, $1BE8, $1BE9, 
        $1BED, $1BEF..$1BF1, $1C2C..$1C33, $1C36, $1C37, $1CD0..$1CD2, $1CD4..$1CE0, $1CE2..$1CE8, 
        $1CED, $1CF4, $1CF8, $1CF9, $1DC0..$1DF9, $1DFB..$1DFF: result := ubcNonSpacingMark;
        $1800..$180A, $1940, $1944, $1945, $19DE..$19FF, $1FBD, $1FBF..$1FC1, $1FCD..$1FCF, $1FDD..$1FDF, 
        $1FED..$1FEF, $1FFD, $1FFE: result := ubcOtherNeutrals;
      else result := ubcUnknown;
      end;
    $02000..$02FFF:
      case u of
        $200B..$200D, $2060..$2064, $206A..$206F: result := ubcBoundaryNeutral;
        $2029: result := ubcParagraphSeparator;
        $2000..$200A, $2028, $205F: result := ubcWhiteSpace;
        $200E, $2071, $207F, $2090..$209C, $2102, $2107, $210A..$2113, $2115, $2119..$211D, $2124, 
        $2126, $2128, $212A..$212D, $212F..$2139, $213C..$213F, $2145..$2149, $214E, $214F, $2160..$2188, 
        $2336..$237A, $2395, $249C..$24E9, $26AC: result := ubcLeftToRight;
        $2800..$28FF, $2C00..$2C2E, $2C30..$2C5E, $2C60..$2CE4, $2CEB..$2CEE, $2CF2, $2CF3, $2D00..$2D25, 
        $2D27, $2D2D, $2D30..$2D67, $2D6F, $2D70, $2D80..$2D96, $2DA0..$2DA6, $2DA8..$2DAE, $2DB0..$2DB6, 
        $2DB8..$2DBE, $2DC0..$2DC6, $2DC8..$2DCE, $2DD0..$2DD6, $2DD8..$2DDE: result := ubcLeftToRight;
        $200F: result := ubcRightToLeft;
        $2070, $2074..$2079, $2080..$2089, $2488..$249B: result := ubcEuropeanNumber;
        $207A, $207B, $208A, $208B, $2212: result := ubcEuropeanNumberSeparator;
        $2030..$2034, $20A0..$20BF, $212E, $2213: result := ubcEuropeanNumberTerminator;
        $202F, $2044: result := ubcCommonSeparator;
        $20D0..$20F0, $2CEF..$2CF1, $2D7F, $2DE0..$2DFF: result := ubcNonSpacingMark;
        $2010..$2027, $2035..$2038, $203B..$2043, $2047..$205E, $207C, $208C, $2100, $2101, $2103..$2106, 
        $2108, $2109, $2114, $2116..$2118, $211E..$2123, $2125, $2127, $2129, $213A, $213B, $2141..$2144, 
        $214A..$214D, $2150..$215F, $2189..$218B, $2190..$2200, $2205: result := ubcOtherNeutrals;
        $2039, $203A, $2045, $2046, $207D, $207E, $208D, $208E, $2140, $2201..$2204, $2208..$220D, 
        $2211, $2215, $2216, $221A..$221D, $221F..$2222, $2224, $2226, $222B..$2233, $2239, $223B..$224C, 
        $2252..$2255, $225F, $2260, $2262, $2264..$226B, $226E..$228C, $228F: result := ubcMirroredNeutral;
        $2206, $2207, $220E..$2210, $2214, $2217..$2219, $221E, $2223, $2225, $2227..$222A, $2234..$2238, 
        $223A, $224D..$2251, $2256..$225E, $2261, $2263, $226C, $226D, $228D, $228E, $2293..$2297, 
        $2299..$22A1, $22A4, $22A5, $22B9..$22BD, $22C0..$22C8, $22CE: result := ubcOtherNeutrals;
        $22CF, $22D2..$22D5, $22EE, $22EF, $2300..$2307, $230C..$231F, $2322..$2328, $232B..$2335, 
        $237B..$2394, $2396..$2426, $2440..$244A, $2460..$2487, $24EA..$26AB, $26AD..$2767, $2776..$27BF, 
        $27C1, $27C2, $27C7, $27CA, $27CE..$27D2, $27D7..$27DB, $27DF..$27E1, $27F0..$27FF, $2900: result := ubcOtherNeutrals;
        $2290..$2292, $2298, $22A2, $22A3, $22A6..$22B8, $22BE, $22BF, $22C9..$22CD, $22D0, $22D1, 
        $22D6..$22ED, $22F0..$22FF, $2308..$230B, $2320, $2321, $2329, $232A, $2768..$2775, $27C0, 
        $27C3..$27C6, $27C8, $27C9, $27CB..$27CD, $27D3..$27D6, $27DC..$27DE, $27E2..$27EF, $2983..$2998, 
        $299B: result := ubcMirroredNeutral;
        $2901..$2982, $2999, $299A, $29A1, $29B0..$29B7, $29B9..$29BF, $29C6..$29C8, $29CA..$29CD, 
        $29D3, $29D6, $29D7, $29DD..$29E0, $29E2, $29E6, $29E7, $29EA..$29F3, $29FA, $29FB, $29FE..$2A09, 
        $2A1D, $2A22, $2A23, $2A25, $2A27, $2A28, $2A2A, $2A2F..$2A33, $2A36: result := ubcOtherNeutrals;
        $299C..$29A0, $29A2..$29AF, $29B8, $29C0..$29C5, $29C9, $29CE..$29D2, $29D4, $29D5, $29D8..$29DC, 
        $29E1, $29E3..$29E5, $29E8, $29E9, $29F4..$29F9, $29FC, $29FD, $2A0A..$2A1C, $2A1E..$2A21, 
        $2A24, $2A26, $2A29, $2A2B..$2A2E, $2A34, $2A35, $2A3C..$2A3E, $2A57: result := ubcMirroredNeutral;
        $2A37..$2A3B, $2A3F..$2A56, $2A59..$2A63, $2A66..$2A69, $2A6E, $2A71, $2A72, $2A75..$2A78, 
        $2AA4, $2AA5, $2AAE, $2AD7..$2ADB, $2ADD, $2ADF..$2AE1, $2AE7..$2AEB, $2AEF..$2AF2, $2AF4..$2AF6, 
        $2AFC, $2AFE..$2B73, $2B76..$2B95, $2B97..$2BFD, $2BFF, $2CE5..$2CEA, $2CF9: result := ubcOtherNeutrals;
        $2CFA..$2CFF, $2E00, $2E01, $2E06..$2E08, $2E0B, $2E0E..$2E1B, $2E1E, $2E1F, $2E2A..$2E52, 
        $2E80..$2E99, $2E9B..$2EF3, $2F00..$2FD5, $2FF0..$2FFB: result := ubcOtherNeutrals;
        $2A58, $2A64, $2A65, $2A6A..$2A6D, $2A6F, $2A70, $2A73, $2A74, $2A79..$2AA3, $2AA6..$2AAD, 
        $2AAF..$2AD6, $2ADC, $2ADE, $2AE2..$2AE6, $2AEC..$2AEE, $2AF3, $2AF7..$2AFB, $2AFD, $2BFE, 
        $2E02..$2E05, $2E09, $2E0A, $2E0C, $2E0D, $2E1C, $2E1D, $2E20..$2E29: result := ubcMirroredNeutral;
      else result := ubcUnknown;
      end;
    else
      case u of
        $3000: result := ubcWhiteSpace;
        $3005..$3007, $3021..$3029, $3031..$3035, $3038..$303C, $3041..$3096, $309D..$309F, $30A1..$30FA, 
        $30FC..$30FF, $3105..$312F, $3131..$318E, $3190..$31BF, $31F0..$321C, $3220..$324F, $3260..$327B, 
        $327F..$32B0, $32C0..$32CB, $32D0..$3376, $337B..$33DD, $33E0..$33FE, $3400, $4DBF, $4E00: result := ubcLeftToRight;
        $302E, $302F: result := ubcCombiningLeftToRight;
        $302A..$302D, $3099, $309A: result := ubcNonSpacingMark;
        $3001..$3004, $3012, $3013, $301C..$3020, $3030, $3036, $3037, $303D..$303F, $309B, $309C, 
        $30A0, $30FB, $31C0..$31E3, $321D, $321E, $3250..$325F, $327C..$327E, $32B1..$32BF, $32CC..$32CF, 
        $3377..$337A, $33DE, $33DF, $33FF, $4DC0..$4DFF: result := ubcOtherNeutrals;
        $3008..$3011, $3014..$301B: result := ubcMirroredNeutral;
      else result := ubcUnknown;
      end;
    end;
  $08000..$0BFFF:
      case u of
        $9FFC, $A000..$A48C, $A4D0..$A60C, $A610..$A62B, $A640..$A66E, $A680..$A69D, $A6A0..$A6EF, 
        $A6F2..$A6F7, $A722..$A787, $A789..$A7BF, $A7C2..$A7CA, $A7F5..$A801, $A803..$A805, $A807..$A80A, 
        $A80C..$A822, $A830..$A837, $A840..$A873, $A882..$A8B3, $A8CE..$A8D9, $A8F2..$A8FE, $A900..$A925, 
        $A92E: result := ubcLeftToRight;
        $A92F..$A946, $A95F..$A97C, $A984..$A9B2, $A9C1..$A9CD, $A9CF..$A9D9, $A9DE..$A9E4, $A9E6..$A9FE, 
        $AA00..$AA28, $AA40..$AA42, $AA44..$AA4B, $AA50..$AA59, $AA5C..$AA7A, $AA7E..$AAAF, $AAB1, 
        $AAB5, $AAB6, $AAB9..$AABD, $AAC0, $AAC2, $AADB..$AAEA, $AAF0..$AAF4, $AB01..$AB06, $AB09: result := ubcLeftToRight;
        $AB0A..$AB0E, $AB11..$AB16, $AB20..$AB26, $AB28..$AB2E, $AB30..$AB69, $AB70..$ABE2, $ABEB, 
        $ABF0..$ABF9, $AC00: result := ubcLeftToRight;
        $A823, $A824, $A827, $A880, $A881, $A8B4..$A8C3, $A952, $A953, $A983, $A9B4, $A9B5, $A9BA, 
        $A9BB, $A9BE..$A9C0, $AA2F, $AA30, $AA33, $AA34, $AA4D, $AA7B, $AA7D, $AAEB, $AAEE, $AAEF, 
        $AAF5, $ABE3, $ABE4, $ABE6, $ABE7, $ABE9, $ABEA, $ABEC: result := ubcCombiningLeftToRight;
        $A838, $A839: result := ubcEuropeanNumberTerminator;
        $A66F..$A672, $A674..$A67D, $A69E, $A69F, $A6F0, $A6F1, $A802, $A806, $A80B, $A825, $A826, 
        $A82C, $A8C4, $A8C5, $A8E0..$A8F1, $A8FF, $A926..$A92D, $A947..$A951, $A980..$A982, $A9B3, 
        $A9B6..$A9B9, $A9BC, $A9BD, $A9E5, $AA29..$AA2E, $AA31, $AA32, $AA35: result := ubcNonSpacingMark;
        $AA36, $AA43, $AA4C, $AA7C, $AAB0, $AAB2..$AAB4, $AAB7, $AAB8, $AABE, $AABF, $AAC1, $AAEC, 
        $AAED, $AAF6, $ABE5, $ABE8, $ABED: result := ubcNonSpacingMark;
        $A490..$A4C6, $A60D..$A60F, $A673, $A67E, $A67F, $A700..$A721, $A788, $A828..$A82B, $A874..$A877, 
        $AB6A, $AB6B: result := ubcOtherNeutrals;
      else result := ubcUnknown;
      end;
  $0C000..$0FFFF:
      case u of
        $FEFF: result := ubcBoundaryNeutral;
        $D7A3, $D7B0..$D7C6, $D7CB..$D7FB, $D800, $DB7F, $DB80, $DBFF, $DC00, $DFFF, $E000, $F8FF..$FA6D, 
        $FA70..$FAD9, $FB00..$FB06, $FB13..$FB17, $FF21..$FF3A, $FF41..$FF5A, $FF66..$FFBE, $FFC2..$FFC7, 
        $FFCA..$FFCF, $FFD2..$FFD7, $FFDA..$FFDC: result := ubcLeftToRight;
        $FB1D, $FB1F..$FB28, $FB2A..$FB36, $FB38..$FB3C, $FB3E, $FB40, $FB41, $FB43, $FB44, $FB46..$FB4F: result := ubcRightToLeft;
        $FB50..$FBC1, $FBD3..$FD3D, $FD50..$FD8F, $FD92..$FDC7, $FDF0..$FDFC, $FE70..$FE74, $FE76..$FEFC: result := ubcArabicLetter;
        $FF10..$FF19: result := ubcEuropeanNumber;
        $FB29, $FE62, $FE63, $FF0B, $FF0D: result := ubcEuropeanNumberSeparator;
        $FE5F, $FE69, $FE6A, $FF03..$FF05, $FFE0, $FFE1, $FFE5, $FFE6: result := ubcEuropeanNumberTerminator;
        $FE50, $FE52, $FE55, $FF0C, $FF0E, $FF0F, $FF1A: result := ubcCommonSeparator;
        $FB1E, $FE00..$FE0F, $FE20..$FE2F: result := ubcNonSpacingMark;
        $FD3E, $FD3F, $FDFD, $FE10..$FE19, $FE30..$FE4F, $FE51, $FE54, $FE56..$FE58, $FE60, $FE61, 
        $FE66, $FE68, $FE6B, $FF01, $FF02, $FF06, $FF07, $FF0A, $FF1B, $FF1D, $FF1F, $FF20, $FF3C, 
        $FF3E..$FF40, $FF5C, $FF5E, $FF61: result := ubcOtherNeutrals;
        $FF64, $FF65, $FFE2..$FFE4, $FFE8..$FFEE, $FFF9..$FFFD: result := ubcOtherNeutrals;
        $FE59..$FE5E, $FE64, $FE65, $FF08, $FF09, $FF1C, $FF1E, $FF3B, $FF3D, $FF5B, $FF5D, $FF5F, 
        $FF60, $FF62, $FF63: result := ubcMirroredNeutral;
      else result := ubcUnknown;
      end;
  else
    case u of
    $10000..$107FF:
      case u of
        $10000..$1000B, $1000D..$10026, $10028..$1003A, $1003C, $1003D, $1003F..$1004D, $10050..$1005D, 
        $10080..$100FA, $10100, $10102, $10107..$10133, $10137..$1013F, $1018D, $1018E, $101D0..$101FC, 
        $10280..$1029C, $102A0..$102D0, $10300..$10323, $1032D..$1034A, $10350..$10375, $10380..$1039D, 
        $1039F..$103C3, $103C8..$103D5, $10400: result := ubcLeftToRight;
        $10401..$1049D, $104A0..$104A9, $104B0..$104D3, $104D8..$104FB, $10500..$10527, $10530..$10563, 
        $1056F, $10600..$10736, $10740..$10755, $10760..$10767: result := ubcLeftToRight;
        $102E1..$102FB: result := ubcEuropeanNumber;
        $101FD, $102E0, $10376..$1037A: result := ubcNonSpacingMark;
        $10101, $10140..$1018C, $10190..$1019C, $101A0: result := ubcOtherNeutrals;
      else result := ubcUnknown;
      end;
    $10800..$10FFF:
      case u of
        $10800..$10805, $10808, $1080A..$10835, $10837, $10838, $1083C, $1083F..$10855, $10857..$1089E, 
        $108A7..$108AF, $108E0..$108F2, $108F4, $108F5, $108FB..$1091B, $10920..$10939, $1093F, 
        $10980..$109B7, $109BC..$109CF, $109D2..$10A00, $10A10..$10A13, $10A15..$10A17, $10A19..$10A35, 
        $10A40..$10A48, $10A50..$10A58, $10A60: result := ubcRightToLeft;
        $10A61..$10A9F, $10AC0..$10AE4, $10AEB..$10AF6, $10B00..$10B35, $10B40..$10B55, $10B58..$10B72, 
        $10B78..$10B91, $10B99..$10B9C, $10BA9..$10BAF, $10C00..$10C48, $10C80..$10CB2, $10CC0..$10CF2, 
        $10CFA..$10CFF, $10E80..$10EA9, $10EAD, $10EB0, $10EB1, $10F00..$10F27, $10FB0..$10FCB, 
        $10FE0..$10FF6: result := ubcRightToLeft;
        $10D00..$10D23, $10F30..$10F45, $10F51..$10F59: result := ubcArabicLetter;
        $10D30..$10D39, $10E60..$10E7E: result := ubcArabicNumber;
        $10A01..$10A03, $10A05, $10A06, $10A0C..$10A0F, $10A38..$10A3A, $10A3F, $10AE5, $10AE6, 
        $10D24..$10D27, $10EAB, $10EAC, $10F46..$10F50: result := ubcNonSpacingMark;
        $1091F, $10B39..$10B3F: result := ubcOtherNeutrals;
      else result := ubcUnknown;
      end;
    $11000..$117FF:
      case u of
        $11003..$11037, $11047..$1104D, $11066..$1106F, $11083..$110AF, $110BB..$110C1, $110CD, 
        $110D0..$110E8, $110F0..$110F9, $11103..$11126, $11136..$11144, $11147, $11150..$11172, 
        $11174..$11176, $11183..$111B2, $111C1..$111C8, $111CD, $111D0..$111DF, $111E1..$111F4, 
        $11200..$11211, $11213..$1122B, $11238..$1123D, $11280: result := ubcLeftToRight;
        $11000, $11002, $11082, $110B0..$110B2, $110B7, $110B8, $1112C, $11145, $11146, $11182, 
        $111B3..$111B5, $111BF, $111C0, $111CE, $1122C..$1122E, $11232, $11233, $11235, $112E0..$112E2, 
        $11302, $11303, $1133E, $1133F, $11341..$11344, $11347, $11348, $1134B..$1134D, $11357, 
        $11362: result := ubcCombiningLeftToRight;
        $11281..$11286, $11288, $1128A..$1128D, $1128F..$1129D, $1129F..$112A9, $112B0..$112DE, 
        $112F0..$112F9, $11305..$1130C, $1130F, $11310, $11313..$11328, $1132A..$11330, $11332, 
        $11333, $11335..$11339, $1133D, $11350, $1135D..$11361, $11400..$11434, $11447..$1145B, 
        $1145D, $1145F..$11461, $11480..$114AF, $114C4: result := ubcLeftToRight;
        $114C5..$114C7, $114D0..$114D9, $11580..$115AE, $115C1..$115DB, $11600..$1162F, $11641..$11644, 
        $11650..$11659, $11680..$116AA, $116B8, $116C0..$116C9, $11700..$1171A, $11730..$1173F: result := ubcLeftToRight;
        $11363, $11435..$11437, $11440, $11441, $11445, $114B0..$114B2, $114B9, $114BB..$114BE, 
        $114C1, $115AF..$115B1, $115B8..$115BB, $115BE, $11630..$11632, $1163B, $1163C, $1163E, 
        $116AC, $116AE, $116AF, $116B6, $11720, $11721, $11726: result := ubcCombiningLeftToRight;
        $11001, $11038..$11046, $1107F..$11081, $110B3..$110B6, $110B9, $110BA, $11100..$11102, 
        $11127..$1112B, $1112D..$11134, $11173, $11180, $11181, $111B6..$111BE, $111C9..$111CC, 
        $111CF, $1122F..$11231, $11234, $11236, $11237, $1123E, $112DF, $112E3..$112EA, $11300, 
        $11301, $1133B, $1133C, $11340: result := ubcNonSpacingMark;
        $11366..$1136C, $11370..$11374, $11438..$1143F, $11442..$11444, $11446, $1145E, $114B3..$114B8, 
        $114BA, $114BF, $114C0, $114C2, $114C3, $115B2..$115B5, $115BC, $115BD, $115BF, $115C0, 
        $115DC, $115DD, $11633..$1163A, $1163D, $1163F, $11640, $116AB, $116AD, $116B0..$116B5, 
        $116B7, $1171D: result := ubcNonSpacingMark;
        $1171E, $1171F, $11722..$11725, $11727..$1172B: result := ubcNonSpacingMark;
        $11052..$11065, $11660..$1166C: result := ubcOtherNeutrals;
      else result := ubcUnknown;
      end;
    $11800..$17FFF:
      case u of
        $11800..$1182B, $1183B, $118A0..$118F2, $118FF..$11906, $11909, $1190C..$11913, $11915, 
        $11916, $11918..$1192F, $1193F, $11941, $11944..$11946, $11950..$11959, $119A0..$119A7, 
        $119AA..$119D0, $119E1..$119E3, $11A00, $11A07, $11A08, $11A0B..$11A32, $11A3A, $11A3F..$11A46, 
        $11A50, $11A5C: result := ubcLeftToRight;
        $1182C..$1182E, $11838, $11930..$11935, $11937, $11938, $1193D, $11940, $11942, $119D1..$119D3, 
        $119DC..$119DF, $119E4, $11A39, $11A57, $11A58, $11A97, $11C2F, $11C3E, $11CA9, $11CB1, 
        $11CB4, $11D8A..$11D8E, $11D93, $11D94, $11D96, $11EF5: result := ubcCombiningLeftToRight;
        $11A5D..$11A89, $11A9A..$11AA2, $11AC0..$11AF8, $11C00..$11C08, $11C0A..$11C2E, $11C3F..$11C45, 
        $11C50..$11C6C, $11C70..$11C8F, $11D00..$11D06, $11D08, $11D09, $11D0B..$11D30, $11D46, 
        $11D50..$11D59, $11D60..$11D65, $11D67, $11D68, $11D6A..$11D89, $11D98, $11DA0..$11DA9, 
        $11EE0..$11EF2, $11EF7, $11EF8, $11FB0, $11FC0: result := ubcLeftToRight;
        $11FC1..$11FD4, $11FFF..$12399, $12400..$1246E, $12470..$12474, $12480..$12543, $13000..$1342E, 
        $13430..$13438, $14400..$14646, $16800..$16A38, $16A40..$16A5E, $16A60..$16A69, $16A6E, 
        $16A6F, $16AD0..$16AED, $16AF5, $16B00..$16B2F, $16B37..$16B45, $16B50..$16B59, $16B5B..$16B61, 
        $16B63..$16B77, $16B7D..$16B8F, $16E40..$16E9A, $16F00: result := ubcLeftToRight;
        $16F01..$16F4A, $16F50, $16F93..$16F9F, $16FE0, $16FE1, $16FE3, $17000: result := ubcLeftToRight;
        $11EF6, $16F51..$16F87, $16FF0, $16FF1: result := ubcCombiningLeftToRight;
        $11FDD..$11FE0: result := ubcEuropeanNumberTerminator;
        $1182F..$11837, $11839, $1183A, $1193B, $1193C, $1193E, $11943, $119D4..$119D7, $119DA, 
        $119DB, $119E0, $11A01..$11A06, $11A09, $11A0A, $11A33..$11A38, $11A3B..$11A3E, $11A47, 
        $11A51..$11A56, $11A59..$11A5B, $11A8A..$11A96, $11A98, $11A99, $11C30..$11C36, $11C38..$11C3D, 
        $11C92..$11CA7, $11CAA..$11CB0, $11CB2: result := ubcNonSpacingMark;
        $11CB3, $11CB5, $11CB6, $11D31..$11D36, $11D3A, $11D3C, $11D3D, $11D3F..$11D45, $11D47, 
        $11D90, $11D91, $11D95, $11D97, $11EF3, $11EF4, $16AF0..$16AF4, $16B30..$16B36, $16F4F, 
        $16F8F..$16F92, $16FE4: result := ubcNonSpacingMark;
        $11FD5..$11FDC, $11FE1..$11FF1, $16FE2: result := ubcOtherNeutrals;
      else result := ubcUnknown;
      end;
    $18000..$1DFFF:
      case u of
        $1BCA0..$1BCA3, $1D173..$1D17A: result := ubcBoundaryNeutral;
        $187F7, $18800..$18CD5, $18D00, $18D08, $1B000..$1B11E, $1B150..$1B152, $1B164..$1B167, 
        $1B170..$1B2FB, $1BC00..$1BC6A, $1BC70..$1BC7C, $1BC80..$1BC88, $1BC90..$1BC99, $1BC9C, 
        $1BC9F, $1D000..$1D0F5, $1D100..$1D126, $1D129..$1D164, $1D16A..$1D16C, $1D183, $1D184, 
        $1D18C..$1D1A9, $1D1AE..$1D1E8, $1D2E0: result := ubcLeftToRight;
        $1D2E1..$1D2F3, $1D360..$1D378, $1D400..$1D454, $1D456..$1D49C, $1D49E, $1D49F, $1D4A2, 
        $1D4A5, $1D4A6, $1D4A9..$1D4AC, $1D4AE..$1D4B9, $1D4BB, $1D4BD..$1D4C3, $1D4C5..$1D505, 
        $1D507..$1D50A, $1D50D..$1D514, $1D516..$1D51C, $1D51E..$1D539, $1D53B..$1D53E, $1D540..$1D544, 
        $1D546, $1D54A..$1D550, $1D552..$1D6A5, $1D6A8: result := ubcLeftToRight;
        $1D6A9..$1D6DA, $1D6DC..$1D714, $1D716..$1D74E, $1D750..$1D788, $1D78A..$1D7C2, $1D7C4..$1D7CB, 
        $1D800..$1D9FF, $1DA37..$1DA3A, $1DA6D..$1DA74, $1DA76..$1DA83, $1DA85..$1DA8B: result := ubcLeftToRight;
        $1D165, $1D166, $1D16D..$1D172: result := ubcCombiningLeftToRight;
        $1D7CE..$1D7FF: result := ubcEuropeanNumber;
        $1BC9D, $1BC9E, $1D167..$1D169, $1D17B..$1D182, $1D185..$1D18B, $1D1AA..$1D1AD, $1D242..$1D244, 
        $1DA00..$1DA36, $1DA3B..$1DA6C, $1DA75, $1DA84, $1DA9B..$1DA9F, $1DAA1..$1DAAF: result := ubcNonSpacingMark;
        $1D200..$1D241, $1D245, $1D300..$1D356: result := ubcOtherNeutrals;
        $1D6DB, $1D715, $1D74F, $1D789, $1D7C3: result := ubcMirroredNeutral;
      else result := ubcUnknown;
      end;
    $1E000..$FFFFF:
      case u of
        $E0001, $E0020..$E007F: result := ubcBoundaryNeutral;
        $1EC71..$1ECB4, $1ED01..$1ED3D, $1EE00..$1EE03, $1EE05..$1EE1F, $1EE21, $1EE22, $1EE24, 
        $1EE27, $1EE29..$1EE32, $1EE34..$1EE37, $1EE39, $1EE3B, $1EE42, $1EE47, $1EE49, $1EE4B, 
        $1EE4D..$1EE4F, $1EE51, $1EE52, $1EE54, $1EE57, $1EE59, $1EE5B, $1EE5D: result := ubcArabicLetter;
        $1E100..$1E12C, $1E137..$1E13D, $1E140..$1E149, $1E14E, $1E14F, $1E2C0..$1E2EB, $1E2F0..$1E2F9, 
        $1F110..$1F12E, $1F130..$1F169, $1F170..$1F1AC, $1F1E6..$1F202, $1F210..$1F23B, $1F240..$1F248, 
        $1F250, $1F251, $20000, $2A6DD, $2A700, $2B734, $2B740, $2B81D, $2B820, $2CEA1, $2CEB0: result := ubcLeftToRight;
        $2EBE0, $2F800..$2FA1D, $30000, $3134A, $F0000, $FFFFD: result := ubcLeftToRight;
        $1E800..$1E8C4, $1E8C7..$1E8CF, $1E900..$1E943, $1E94B, $1E950..$1E959, $1E95E, $1E95F: result := ubcRightToLeft;
        $1EE5F, $1EE61, $1EE62, $1EE64, $1EE67..$1EE6A, $1EE6C..$1EE72, $1EE74..$1EE77, $1EE79..$1EE7C, 
        $1EE7E, $1EE80..$1EE89, $1EE8B..$1EE9B, $1EEA1..$1EEA3, $1EEA5..$1EEA9, $1EEAB..$1EEBB: result := ubcArabicLetter;
        $1F100..$1F10A, $1FBF0..$1FBF9: result := ubcEuropeanNumber;
        $1E2FF: result := ubcEuropeanNumberTerminator;
        $1E000..$1E006, $1E008..$1E018, $1E01B..$1E021, $1E023, $1E024, $1E026..$1E02A, $1E130..$1E136, 
        $1E2EC..$1E2EF, $1E8D0..$1E8D6, $1E944..$1E94A, $E0100..$E01EF: result := ubcNonSpacingMark;
        $1EEF0, $1EEF1, $1F000..$1F02B, $1F030..$1F093, $1F0A0..$1F0AE, $1F0B1..$1F0BF, $1F0C1..$1F0CF, 
        $1F0D1..$1F0F5, $1F10B..$1F10F, $1F12F, $1F16A..$1F16F, $1F1AD, $1F260..$1F265, $1F300..$1F6D7, 
        $1F6E0..$1F6EC, $1F6F0..$1F6FC, $1F700..$1F773, $1F780..$1F7D8, $1F7E0..$1F7EB, $1F800..$1F80B, 
        $1F810..$1F847, $1F850..$1F859, $1F860: result := ubcOtherNeutrals;
        $1F861..$1F887, $1F890..$1F8AD, $1F8B0, $1F8B1, $1F900..$1F978, $1F97A..$1F9CB, $1F9CD..$1FA53, 
        $1FA60..$1FA6D, $1FA70..$1FA74, $1FA78..$1FA7A, $1FA80..$1FA86, $1FA90..$1FAA8, $1FAB0..$1FAB6, 
        $1FAC0..$1FAC2, $1FAD0..$1FAD6, $1FB00..$1FB92, $1FB94..$1FBCA: result := ubcOtherNeutrals;
      else result := ubcUnknown;
      end;
    else result := ubcUnknown;
    end
  end
end;

type
  TUnicodeBracketInfo = record
    IsBracket: boolean;
    OpeningBracket,ClosingBracket: LongWord;
  end;
function GetUnicodeBracketInfo(u: LongWord): TUnicodeBracketInfo;
  procedure Bracket(AOpening,AClosing: LongWord);
  begin
    result.IsBracket := true;
    result.OpeningBracket := AOpening;
    result.ClosingBracket := AClosing;
  end;
begin
  case u of
  $0028, $0029: Bracket($0028, $0029);
  $005B, $005D: Bracket($005B, $005D);
  $007B, $007D: Bracket($007B, $007D);
  $0F3A, $0F3B: Bracket($0F3A, $0F3B);
  $0F3C, $0F3D: Bracket($0F3C, $0F3D);
  $169B, $169C: Bracket($169B, $169C);
  $2045, $2046: Bracket($2045, $2046);
  $207D, $207E: Bracket($207D, $207E);
  $208D, $208E: Bracket($208D, $208E);
  $2308, $2309: Bracket($2308, $2309);
  $230A, $230B: Bracket($230A, $230B);
  $2329, $232A: Bracket($2329, $232A);
  $2768, $2769: Bracket($2768, $2769);
  $276A, $276B: Bracket($276A, $276B);
  $276C, $276D: Bracket($276C, $276D);
  $276E, $276F: Bracket($276E, $276F);
  $2770, $2771: Bracket($2770, $2771);
  $2772, $2773: Bracket($2772, $2773);
  $2774, $2775: Bracket($2774, $2775);
  $27C5, $27C6: Bracket($27C5, $27C6);
  $27E6, $27E7: Bracket($27E6, $27E7);
  $27E8, $27E9: Bracket($27E8, $27E9);
  $27EA, $27EB: Bracket($27EA, $27EB);
  $27EC, $27ED: Bracket($27EC, $27ED);
  $27EE, $27EF: Bracket($27EE, $27EF);
  $2983, $2984: Bracket($2983, $2984);
  $2985, $2986: Bracket($2985, $2986);
  $2987, $2988: Bracket($2987, $2988);
  $2989, $298A: Bracket($2989, $298A);
  $298B, $298C: Bracket($298B, $298C);
  $298D, $2990: Bracket($298D, $2990);
  $298F, $298E: Bracket($298F, $298E);
  $2991, $2992: Bracket($2991, $2992);
  $2993, $2994: Bracket($2993, $2994);
  $2995, $2996: Bracket($2995, $2996);
  $2997, $2998: Bracket($2997, $2998);
  $29D8, $29D9: Bracket($29D8, $29D9);
  $29DA, $29DB: Bracket($29DA, $29DB);
  $29FC, $29FD: Bracket($29FC, $29FD);
  $2E22, $2E23: Bracket($2E22, $2E23);
  $2E24, $2E25: Bracket($2E24, $2E25);
  $2E26, $2E27: Bracket($2E26, $2E27);
  $2E28, $2E29: Bracket($2E28, $2E29);
  $3008, $3009: Bracket($3008, $3009);
  $300A, $300B: Bracket($300A, $300B);
  $300C, $300D: Bracket($300C, $300D);
  $300E, $300F: Bracket($300E, $300F);
  $3010, $3011: Bracket($3010, $3011);
  $3014, $3015: Bracket($3014, $3015);
  $3016, $3017: Bracket($3016, $3017);
  $3018, $3019: Bracket($3018, $3019);
  $301A, $301B: Bracket($301A, $301B);
  $FE59, $FE5A: Bracket($FE59, $FE5A);
  $FE5B, $FE5C: Bracket($FE5B, $FE5C);
  $FE5D, $FE5E: Bracket($FE5D, $FE5E);
  $FF08, $FF09: Bracket($FF08, $FF09);
  $FF3B, $FF3D: Bracket($FF3B, $FF3D);
  $FF5B, $FF5D: Bracket($FF5B, $FF5D);
  $FF5F, $FF60: Bracket($FF5F, $FF60);
  $FF62, $FF63: Bracket($FF62, $FF63);
  else
    begin
      result.IsBracket := false;
      result.OpeningBracket := 0;
      result.ClosingBracket := 0;
    end;
  end;
end;

function GetUnicodeJoiningType(u: LongWord): TUnicodeJoiningType;
begin
  result := ujtNonJoining;
  if u <= $001BE8 then begin
    if u <= $0009CD then begin
      if u <= $00072C then begin
        if u <= $00069A then
          case u of
            $AD, $300..$36F, $483..$489, $591..$5BD, $5BF, $5C1, $5C2, $5C4, $5C5, $5C7, $610..$61A, 
            $61C, $64B..$65F, $670: result := ujtTransparent;
            $622..$625, $627, $629, $62F..$632, $648, $671..$673, $675..$677, $688..$699: result := ujtRightJoining;
            $620, $626, $628, $62A..$62E, $633..$63F, $641..$647, $649, $64A, $66E, $66F, $678..$687, 
            $69A: result := ujtDualJoining;
            $640: result := ujtJoinCausing;
          end
        else
          case u of
            $6D6..$6DC, $6DF..$6E4, $6E7, $6E8, $6EA..$6ED, $70F, $711: result := ujtTransparent;
            $6C0, $6C3..$6CB, $6CD, $6CF, $6D2, $6D3, $6D5, $6EE, $6EF, $710, $715..$719, $71E, 
            $728, $72A, $72C: result := ujtRightJoining;
            $69B..$6BF, $6C1, $6C2, $6CC, $6CE, $6D0, $6D1, $6FA..$6FC, $6FF, $712..$714, $71A..$71D, 
            $71F..$727, $729, $72B: result := ujtDualJoining;
          end
      end else begin
        if u <= $00084A then
          case u of
            $730..$74A, $7A6..$7B0, $7EB..$7F3, $7FD, $816..$819, $81B..$823, $825..$827, $829..$82D: result := ujtTransparent;
            $72F, $74D, $759..$75B, $76B, $76C, $771, $773, $774, $778, $779, $840, $846, $847, 
            $849: result := ujtRightJoining;
            $72D, $72E, $74E..$758, $75C..$76A, $76D..$770, $772, $775..$777, $77A..$77F, $7CA..$7EA, 
            $841..$845, $848, $84A: result := ujtDualJoining;
            $7FA: result := ujtJoinCausing;
          end
        else
          case u of
            $859..$85B, $8D3..$8E1, $8E3..$902, $93A, $93C, $941..$948, $94D, $951..$957, $962, 
            $963, $981, $9BC, $9C1..$9C4, $9CD: result := ujtTransparent;
            $854, $856..$858, $867, $869, $86A, $8AA..$8AC, $8AE, $8B1, $8B2, $8B9: result := ujtRightJoining;
            $84B..$853, $855, $860, $862..$865, $868, $8A0..$8A9, $8AF, $8B0, $8B3, $8B4, $8B6..$8B8, 
            $8BA..$8C7: result := ujtDualJoining;
          end
      end
    end else begin
      if u <= $000F71 then begin
        if u <= $000C3E then
          case u of
            $9E2, $9E3, $9FE, $A01, $A02, $A3C, $A41, $A42, $A47, $A48, $A4B..$A4D, $A51, $A70, 
            $A71, $A75, $A81, $A82, $ABC, $AC1..$AC5, $AC7, $AC8, $ACD, $AE2, $AE3, $AFA..$AFF, 
            $B01, $B3C, $B3F, $B41..$B44, $B4D, $B55, $B56, $B62, $B63, $B82, $BC0, $BCD, $C00, 
            $C04, $C3E: result := ujtTransparent;
          end
        else
          case u of
            $C3F, $C40, $C46..$C48, $C4A..$C4D, $C55, $C56, $C62, $C63, $C81, $CBC, $CBF, $CC6, 
            $CCC, $CCD, $CE2, $CE3, $D00, $D01, $D3B, $D3C, $D41..$D44, $D4D, $D62, $D63, $D81, 
            $DCA, $DD2..$DD4, $DD6, $E31, $E34..$E3A, $E47..$E4E, $EB1, $EB4..$EBC, $EC8..$ECD, 
            $F18, $F19, $F35, $F37, $F39, $F71: result := ujtTransparent;
          end
      end else begin
        if u <= $001820 then
          case u of
            $F72..$F7E, $F80..$F84, $F86, $F87, $F8D..$F97, $F99..$FBC, $FC6, $102D..$1030, $1032..$1037, 
            $1039, $103A, $103D, $103E, $1058, $1059, $105E..$1060, $1071..$1074, $1082, $1085, 
            $1086, $108D, $109D, $135D..$135F, $1712..$1714, $1732..$1734, $1752, $1753, $1772, 
            $1773, $17B4, $17B5, $17B7..$17BD, $17C6, $17C9..$17D3, $17DD, $180B..$180D: result := ujtTransparent;
            $1807, $1820: result := ujtDualJoining;
            $180A: result := ujtJoinCausing;
          end
        else
          case u of
            $1885, $1886, $18A9, $1920..$1922, $1927, $1928, $1932, $1939..$193B, $1A17, $1A18, 
            $1A1B, $1A56, $1A58..$1A5E, $1A60, $1A62, $1A65..$1A6C, $1A73..$1A7C, $1A7F, $1AB0..$1AC0, 
            $1B00..$1B03, $1B34, $1B36..$1B3A, $1B3C, $1B42, $1B6B..$1B73, $1B80, $1B81, $1BA2..$1BA5, 
            $1BA8, $1BA9, $1BAB..$1BAD, $1BE6, $1BE8: result := ujtTransparent;
            $1821..$1878, $1887..$18A8, $18AA: result := ujtDualJoining;
          end
      end
    end
  end else begin
    if u <= $010FB8 then begin
      if u <= $00ABE8 then begin
        if u <= $00A806 then
          case u of
            $1BE9, $1BED, $1BEF..$1BF1, $1C2C..$1C33, $1C36, $1C37, $1CD0..$1CD2, $1CD4..$1CE0, 
            $1CE2..$1CE8, $1CED, $1CF4, $1CF8, $1CF9, $1DC0..$1DF9, $1DFB..$1DFF, $200B, $200E, 
            $200F, $202A..$202E, $2060..$2064, $206A..$206F, $20D0..$20F0, $2CEF..$2CF1, $2D7F, 
            $2DE0..$2DFF, $302A..$302D, $3099, $309A, $A66F..$A672, $A674..$A67D, $A69E, $A69F, 
            $A6F0, $A6F1, $A802, $A806: result := ujtTransparent;
            $200D: result := ujtJoinCausing;
          end
        else
          case u of
            $A80B, $A825, $A826, $A82C, $A8C4, $A8C5, $A8E0..$A8F1, $A8FF, $A926..$A92D, $A947..$A951, 
            $A980..$A982, $A9B3, $A9B6..$A9B9, $A9BC, $A9BD, $A9E5, $AA29..$AA2E, $AA31, $AA32, 
            $AA35, $AA36, $AA43, $AA4C, $AA7C, $AAB0, $AAB2..$AAB4, $AAB7, $AAB8, $AABE, $AABF, 
            $AAC1, $AAEC, $AAED, $AAF6, $ABE5, $ABE8: result := ujtTransparent;
            $A872: result := ujtLeftJoining;
            $A840..$A871: result := ujtDualJoining;
          end
      end else begin
        if u <= $010AEF then
          case u of
            $ABED, $FB1E, $FE00..$FE0F, $FE20..$FE2F, $FEFF, $FFF9..$FFFB, $101FD, $102E0, $10376..$1037A, 
            $10A01..$10A03, $10A05, $10A06, $10A0C..$10A0F, $10A38..$10A3A, $10A3F, $10AE5, $10AE6: result := ujtTransparent;
            $10AC5, $10AC7, $10AC9, $10ACA, $10ACE..$10AD2, $10ADD, $10AE1, $10AE4, $10AEF: result := ujtRightJoining;
            $10ACD, $10AD7: result := ujtLeftJoining;
            $10AC0..$10AC4, $10AD3..$10AD6, $10AD8..$10ADC, $10ADE..$10AE0, $10AEB..$10AEE: result := ujtDualJoining;
          end
        else
          case u of
            $10D24..$10D27, $10EAB, $10EAC, $10F46..$10F50: result := ujtTransparent;
            $10B81, $10B83..$10B85, $10B89, $10B8C, $10B8E, $10B8F, $10B91, $10BA9..$10BAC, $10D22, 
            $10F33, $10F54, $10FB4..$10FB6: result := ujtRightJoining;
            $10D00: result := ujtLeftJoining;
            $10B80, $10B82, $10B86..$10B88, $10B8A, $10B8B, $10B8D, $10B90, $10BAD, $10BAE, $10D01..$10D21, 
            $10D23, $10F30..$10F32, $10F34..$10F44, $10F51..$10F53, $10FB0, $10FB2, $10FB3, $10FB8: result := ujtDualJoining;
          end
      end
    end else begin
      if u <= $01193E then begin
        if u <= $011300 then
          case u of
            $11001, $11038..$11046, $1107F..$11081, $110B3..$110B6, $110B9, $110BA, $11100..$11102, 
            $11127..$1112B, $1112D..$11134, $11173, $11180, $11181, $111B6..$111BE, $111C9..$111CC, 
            $111CF, $1122F..$11231, $11234, $11236, $11237, $1123E, $112DF, $112E3..$112EA, $11300: result := ujtTransparent;
            $10FB9, $10FBA, $10FBD, $10FC2, $10FC3, $10FC9: result := ujtRightJoining;
            $10FCB: result := ujtLeftJoining;
            $10FBB, $10FBC, $10FBE, $10FBF, $10FC1, $10FC4, $10FCA: result := ujtDualJoining;
          end
        else
          case u of
            $11301, $1133B, $1133C, $11340, $11366..$1136C, $11370..$11374, $11438..$1143F, $11442..$11444, 
            $11446, $1145E, $114B3..$114B8, $114BA, $114BF, $114C0, $114C2, $114C3, $115B2..$115B5, 
            $115BC, $115BD, $115BF, $115C0, $115DC, $115DD, $11633..$1163A, $1163D, $1163F, $11640, 
            $116AB, $116AD, $116B0..$116B5, $116B7, $1171D..$1171F, $11722..$11725, $11727..$1172B, 
            $1182F..$11837, $11839, $1183A, $1193B, $1193C, $1193E: result := ujtTransparent;
          end
      end else begin
        if u <= $016AF0 then
          case u of
            $11943, $119D4..$119D7, $119DA, $119DB, $119E0, $11A01..$11A0A, $11A33..$11A38, $11A3B..$11A3E, 
            $11A47, $11A51..$11A56, $11A59..$11A5B, $11A8A..$11A96, $11A98, $11A99, $11C30..$11C36, 
            $11C38..$11C3D, $11C3F, $11C92..$11CA7, $11CAA..$11CB0, $11CB2, $11CB3, $11CB5, $11CB6, 
            $11D31..$11D36, $11D3A, $11D3C, $11D3D, $11D3F..$11D45, $11D47, $11D90, $11D91, $11D95, 
            $11D97, $11EF3, $11EF4, $13430..$13438, $16AF0: result := ujtTransparent;
          end
        else
          case u of
            $16AF1..$16AF4, $16B30..$16B36, $16F4F, $16F8F..$16F92, $16FE4, $1BC9D, $1BC9E, $1BCA0..$1BCA3, 
            $1D167..$1D169, $1D173..$1D182, $1D185..$1D18B, $1D1AA..$1D1AD, $1D242..$1D244, $1DA00..$1DA36, 
            $1DA3B..$1DA6C, $1DA75, $1DA84, $1DA9B..$1DA9F, $1DAA1..$1DAAF, $1E000..$1E006, $1E008..$1E018, 
            $1E01B..$1E021, $1E023, $1E024, $1E026..$1E02A, $1E130..$1E136, $1E2EC..$1E2EF, $1E8D0..$1E8D6, 
            $1E944..$1E94B, $E0001, $E0020..$E007F, $E0100..$E01EF: result := ujtTransparent;
            $1E900..$1E943: result := ujtDualJoining;
          end
      end
    end
  end
end;

function GetUnicodeCombiningLayout(u: LongWord): TUnicodeCombiningLayout;
begin
  case u shr 24 of
  $00:
    case u of
    $093F, $094E, $09BF, $09C7..$09C8, $0A3F, $0ABF, $0B47..$0B48, $0B4B..$0B4C, 
    $0BC6..$0BC8, $0D46..$0D48, $0DD9..$0DDE: exit(uclLeft);
    $0903, $093B, $093E, $0940, $0949..$094C, $094F, $0982..$0983, $09BE, 
    $09C0, $09D7, $0A03, $0A3E, $0A40, $0A83, $0ABE, $0AC0, $0AC9, $0ACB..$0ACC, 
    $0B02..$0B03, $0B3E, $0B40, $0B57, $0BBE..$0BBF, $0BC1..$0BC2, $0BD7, 
    $0C01..$0C03, $0C41..$0C44, $0C82..$0C83, $0CBE, $0CC0..$0CC4, $0CC7..$0CC8, 
    $0CCA..$0CCB, $0CD5..$0CD6, $0D02..$0D03, $0D3E..$0D40, $0D57, $0D82..$0D83, 
    $0DCF..$0DD1, $0DD8, $0DDF, $0DF2..$0DF3, $0F3E..$0F3F, $0F7F: exit(uclRight);
    $09CB..$09CC, $0BCA..$0BCC, $0D4A..$0D4C: exit(uclLeftAndRight);
    end;
  $01:
    case u of
    $1031, $103C, $1084, $17BE, $17C1..$17C3, $1A19, $1B3E..$1B3F: exit(uclLeft);
    $102B..$102C, $1038, $103B, $1056..$1057, $1062..$1064, $1067..$106D, 
    $1083, $1087..$108C, $108F, $109A..$109C, $17B6, $17C7..$17C8, $1923..$1926, 
    $1929..$192B, $1930..$1931, $1933..$1938, $1A1A, $1A55, $1A57, $1A61, 
    $1A63..$1A64, $1A6D..$1A72, $1B04, $1B35, $1B3B, $1B43..$1B44, $1B82, 
    $1BA1, $1BA6..$1BA7, $1BAA, $1BE7, $1BEA..$1BEC, $1BEE, $1BF2..$1BF3, 
    $1C24..$1C2B, $1C34..$1C35, $1CE1, $1CF7: exit(uclRight);
    $17BF..$17C0, $17C4..$17C5, $1B3D, $1B40..$1B41: exit(uclLeftAndRight);
    end;
  $03:
    case u of
    $302E..$302F: exit(uclLeft);
    end;
  $0A:
    case u of
    $A9BA..$A9BB, $A9BF, $AA2F..$AA30, $AA34: exit(uclLeft);
    $A823..$A824, $A827, $A880..$A881, $A8B4..$A8C3, $A952..$A953, $A983, 
    $A9B4..$A9B5, $A9BE, $A9C0, $AA33, $AA4D, $AA7B, $AA7D, $AAEB, $AAEE..$AAEF, 
    $AAF5, $ABE3..$ABE4, $ABE6..$ABE7, $ABE9..$ABEA, $ABEC: exit(uclRight);
    end;
  $11:
    case u of
    $11000, $11002, $11082, $110B0..$110B2, $110B7..$110B8, $1112C, $11145..$11146, 
    $11182, $111B3..$111B5, $111BF..$111C0, $111CE, $1122C..$1122E, $11232..$11233, 
    $11235, $112E0..$112E2, $11302..$11303, $1133E..$1133F, $11341..$11344, 
    $11347..$11348, $1134B..$1134D, $11357, $11362..$11363, $11435..$11437, 
    $11440..$11441, $11445, $114B0..$114B2, $114B9, $114BB..$114BE, $114C1, 
    $115AF..$115B1, $115B8..$115BB, $115BE, $11630..$11632, $1163B..$1163C, 
    $1163E, $116AC, $116AE..$116AF, $116B6, $11720..$11721, $11726, $1182C..$1182E, 
    $11838, $11930..$11935, $11937..$11938, $1193D, $11940, $11942, $119D1..$119D3, 
    $119DC..$119DF, $119E4, $11A39, $11A57..$11A58, $11A97, $11C2F, $11C3E, 
    $11CA9, $11CB1, $11CB4, $11D8A..$11D8E, $11D93..$11D94, $11D96, $11EF5..$11EF6: exit(uclRight);
    end;
  $16:
    case u of
    $16F51..$16F87, $16FF0..$16FF1: exit(uclRight);
    end;
  $1D:
    case u of
    $1D165..$1D166, $1D16D..$1D172: exit(uclRight);
    end;
  end;
  result := uclNone;
end;

