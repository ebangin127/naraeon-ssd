unit TestNSTSupport;
{

  Delphi DUnit Test Case
  ----------------------
  This unit contains a skeleton test case class generated by the Test Case Wizard.
  Modify the generated code to correctly setup and call the methods from the unit 
  being tested.

}

interface

uses
  TestFramework,
  Support, Support.Factory;

type
  // Test methods for class TNSTSupport.Factory

  TestTNSTSupport.Factory = class(TTestCase)
  strict private
    FNSTSupport: TNSTSupport;
    procedure TestSupportStatusWithModelFirmware(Model, Firmware: String);
  public
    procedure TearDown; override;
  published
    procedure TestNonSupport;
    //SANDFORCE
    procedure TestHynixSandforceSupport;
    procedure TestMachMXDSFusionSupport;
    procedure TestOCZVertex3Support;
    procedure TestOCZVertex3MaxIOPSSupport;
    procedure TestOCZAgility3Support;
    procedure TestPatriotPyroSupport;
    procedure TestTHNSNSSupport;
    //SUPPORT
    procedure TestCrucialM500Support;
    procedure TestCrucialM550Support;
    procedure TestCrucialMX100Support;
    procedure TestCrucialMX200Support;
    procedure TestLiteonS100Support;
    procedure TestLiteonS200Support;
    procedure TestLiteonE200Support;
    procedure TestMachMXMMYSupport;
    procedure TestMachJetUltraSupport;
    procedure TestPlextorNinjaSupport;
    procedure TestPlextorM3PSupport;
    procedure TestPlextorM5SSupport;
    procedure TestPlextorM5PSupport;
    procedure TestPlextorM5ProSupport;
    procedure TestSamsung470Support;
    procedure TestSamsung830Support;
    procedure TestSamsung840Support;
    procedure TestSamsung840EVOSupport;
    procedure TestSamsung840ProSupport;
    procedure TestSamsung850EVOSupport;
    procedure TestSamsung850ProSupport;
    procedure TestSandiskX110Support;
    procedure TestSeagate600Support;
    procedure TestToshibaQFSupport;
    procedure TestToshibaQHSupport;
    procedure TestToshibaQProSupport;
    procedure TestPhisonCT7Support;
    procedure TestADATASP900Support;
    procedure TestADATASP920Support;
    procedure TestIntel750Support;
    procedure TestSamsung950ProSupport;
  end;

implementation

procedure TestTNSTSupport.Factory.TearDown;
begin
  FNSTSupport.Free;
  FNSTSupport := nil;
end;

procedure TestTNSTSupport.Factory.TestNonSupport;
begin
  FNSTSupport := NSTSupport.Factory.GetSuitableNSTSupport('', '');
  CheckTrue(FNSTSupport = nil, 'Model & Firmware: Blank');
end;

procedure TestTNSTSupport.Factory.TestHynixSandforceSupport;
begin
  TestSupportStatusWithModelFirmware('HYNIX HFS120G3AMNM', '10301A00');
end;

procedure TestTNSTSupport.Factory.TestMachMXDSFusionSupport;
begin
  TestSupportStatusWithModelFirmware('MXSSD3MDSF-120G', '5.04');
end;

procedure TestTNSTSupport.Factory.TestPatriotPyroSupport;
begin
  TestSupportStatusWithModelFirmware('Patriot Pyro', '319ABBF0');
end;

procedure TestTNSTSupport.Factory.TestTHNSNSSupport;
begin
  TestSupportStatusWithModelFirmware('TOSHIBA THNSNS120GBSP', 'TA5ABBF0');
end;

procedure TestTNSTSupport.Factory.TestOCZAgility3Support;
begin
  TestSupportStatusWithModelFirmware('OCZ-AGILITY3', '2.06');
end;

procedure TestTNSTSupport.Factory.TestOCZVertex3MaxIOPSSupport;
begin
  TestSupportStatusWithModelFirmware('OCZ-VERTEX3 MI', '2.22');
end;

procedure TestTNSTSupport.Factory.TestOCZVertex3Support;
begin
  TestSupportStatusWithModelFirmware('OCZ-VERTEX3', '2.15');
end;

procedure TestTNSTSupport.Factory.TestCrucialM500Support;
begin
  TestSupportStatusWithModelFirmware('Crucial_CT120M500SSD1', 'MU05');
  TestSupportStatusWithModelFirmware('Crucial_CT120M500SSD3', 'MU05');
end;

procedure TestTNSTSupport.Factory.TestCrucialM550Support;
begin
  TestSupportStatusWithModelFirmware('Crucial_CT128M550SSD1', 'MU02');
  TestSupportStatusWithModelFirmware('Crucial_CT128M550SSD3', 'MU02');
end;

procedure TestTNSTSupport.Factory.TestCrucialMX100Support;
begin
  TestSupportStatusWithModelFirmware('Crucial_CT128MX100SSD1', 'MU02');
  TestSupportStatusWithModelFirmware('Crucial_CT128MX100SSD3', 'MU02');
end;

procedure TestTNSTSupport.Factory.TestCrucialMX200Support;
begin
  TestSupportStatusWithModelFirmware('Crucial_CT250MX200SSD1', 'MU01');
  TestSupportStatusWithModelFirmware('Crucial_CT250MX200SSD3', 'MU01');
end;

procedure TestTNSTSupport.Factory.TestLiteonE200Support;
begin
  TestSupportStatusWithModelFirmware('LITEONIT E200-160', 'VF81');
end;

procedure TestTNSTSupport.Factory.TestLiteonS100Support;
begin
  TestSupportStatusWithModelFirmware('LITEONIT S100-256', 'VB83');
end;

procedure TestTNSTSupport.Factory.TestLiteonS200Support;
begin
  TestSupportStatusWithModelFirmware('LITEONIT LAM-256M3S', 'WBA1');
end;

procedure TestTNSTSupport.Factory.TestMachJetUltraSupport;
begin
  TestSupportStatusWithModelFirmware('MXSSD2MJTU-128G', 'UNKNOWN');
end;

procedure TestTNSTSupport.Factory.TestMachMXMMYSupport;
begin
  TestSupportStatusWithModelFirmware('MXSSD3MMY-128G', 'V1707');
end;

procedure TestTNSTSupport.Factory.TestPlextorM3PSupport;
begin
  TestSupportStatusWithModelFirmware('PLEXTOR PX-128M3P', '1.01');
end;

procedure TestTNSTSupport.Factory.TestPlextorM5ProSupport;
begin
  TestSupportStatusWithModelFirmware('PLEXTOR PX-128M5Pro', '1.07');
end;

procedure TestTNSTSupport.Factory.TestPlextorM5PSupport;
begin
  TestSupportStatusWithModelFirmware('PLEXTOR PX-128M5P', '1.07');
end;

procedure TestTNSTSupport.Factory.TestPlextorM5SSupport;
begin
  TestSupportStatusWithModelFirmware('PLEXTOR PX-128M5S', '1.05');
end;

procedure TestTNSTSupport.Factory.TestPlextorNinjaSupport;
begin
  TestSupportStatusWithModelFirmware('Ninja-256', '1.00');
end;

procedure TestTNSTSupport.Factory.TestSamsung470Support;
begin
  TestSupportStatusWithModelFirmware('SAMSUNG 470 Series', 'AXM0601Q');
end;

procedure TestTNSTSupport.Factory.TestSamsung830Support;
begin
  TestSupportStatusWithModelFirmware('SAMSUNG SSD 830 Series', 'CXM03B1Q');
end;

procedure TestTNSTSupport.Factory.TestSamsung840EVOSupport;
begin
  TestSupportStatusWithModelFirmware('SAMSUNG SSD 840 EVO 250GB', 'EXT0AB0Q');
end;

procedure TestTNSTSupport.Factory.TestSamsung840ProSupport;
begin
  TestSupportStatusWithModelFirmware('SAMSUNG SSD 840 Pro Series', 'DXM02B0Q');
end;

procedure TestTNSTSupport.Factory.TestSamsung840Support;
begin
  TestSupportStatusWithModelFirmware('SAMSUNG SSD 840 Series', 'DXT06B0Q');
end;

procedure TestTNSTSupport.Factory.TestSamsung850EVOSupport;
begin
  TestSupportStatusWithModelFirmware('SAMSUNG SSD 850 EVO 120GB', 'EMT01B6Q');
end;

procedure TestTNSTSupport.Factory.TestSamsung850ProSupport;
begin
  TestSupportStatusWithModelFirmware('SAMSUNG SSD 850 PRO 512GB', 'EXM01B6Q');
end;

procedure TestTNSTSupport.Factory.TestIntel750Support;
begin
  TestSupportStatusWithModelFirmware('INTEL SSDPEDMW400G4', '8EV10174');
end;

procedure TestTNSTSupport.Factory.TestSamsung950ProSupport;
begin
  TestSupportStatusWithModelFirmware('SAMSUNG SSD 950 PRO 256GB', '1B0QBXX7');
end;

procedure TestTNSTSupport.Factory.TestSandiskX110Support;
begin
  TestSupportStatusWithModelFirmware('SanDisk SD6SB1M256G1022I', 'X230600');
end;

procedure TestTNSTSupport.Factory.TestSeagate600Support;
begin
  TestSupportStatusWithModelFirmware('ST480HM000', 'B660');
end;

procedure TestTNSTSupport.Factory.TestToshibaQFSupport;
begin
  TestSupportStatusWithModelFirmware('TOSHIBA THNSNF256GBSS', 'FSXAN104');
end;

procedure TestTNSTSupport.Factory.TestToshibaQHSupport;
begin
  TestSupportStatusWithModelFirmware('TOSHIBA THNSNH128GBST', 'HTRAN101');
end;

procedure TestTNSTSupport.Factory.TestToshibaQProSupport;
begin
  TestSupportStatusWithModelFirmware('TOSHIBA THNSNJ128GCST', 'JTRA0102');
end;

procedure TestTNSTSupport.Factory.TestPhisonCT7Support;
begin
  TestSupportStatusWithModelFirmware('SATA SSD', 'SAFM00.f');
  TestSupportStatusWithModelFirmware('SATA SSD', 'SAFM01.3');
end;

procedure TestTNSTSupport.Factory.TestADATASP900Support;
begin
  TestSupportStatusWithModelFirmware('ADATA SP900', '5.8.2');
end;

procedure TestTNSTSupport.Factory.TestADATASP920Support;
begin
  TestSupportStatusWithModelFirmware('ADATA SP920SS', 'MU01');
end;

procedure TestTNSTSupport.Factory.TestSupportStatusWithModelFirmware(Model,
  Firmware: String);
begin
  FNSTSupport := NSTSupport.Factory.GetSuitableNSTSupport(Model, Firmware);
  CheckTrue(FNSTSupport <> nil, 'Model: ' + Model + ' Firmware: ' + Firmware);
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTNSTSupport.Factory.Suite);
end.

