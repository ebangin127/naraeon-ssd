unit Device.SlotSpeed;

interface

type
  TPCIeSpecification = (PCIe1d0 = 1, PCIe2d0 = 2, PCIe3d0 = 3);
  TPCIeDataWidth = (PCIex1 = 1, PCIex2 = 2, PCIex4 = 4, PCIex8 = 8,
    PCIex12 = 12, PCIex16 = 16, PCIex32 = 32);
  TSlotSpeed = record
    SpecVersion: TPCIeSpecification;
    LinkWidth: TPCIeDataWidth;
  end;
  TSlotMaxCurrSpeed = record
    Maximum: TSlotSpeed;
    Current: TSlotSpeed;
  end;

const
  SlotSpecificationString: Array[TPCIeSpecification] of String = (
    'PCI Express 1.0', 'PCI Express 2.0', 'PCI Express 3.0');

implementation

end.
