unit Global.LanguageString;

interface

uses Windows;

var
  CurrLang: Integer;

const
  LANG_HANGUL = 0;
  LANG_ENGLISH = 1;

type
  TLanguageString = Array[0..LANG_ENGLISH] of String;

const
  //캐시 오류
    VistaFont: TLanguageString = ('맑은 고딕', 'Segoe UI');
    XPFont: TLanguageString = ('돋움', 'Verdana');

  //캐시 오류
    ErrCache: TLanguageString = ('다음 장치에서 잘못된 캐시 설정이 나타나 수정했습니다', 'Wrong cache setting');

  //메인
    CapConnState: TLanguageString = ('연결 상태 : ', 'Connected State : ');
    CapFirmware: TLanguageString = ('펌웨어 버전 : ', 'Firmware : ');
    CapSerial: TLanguageString = ('시리얼 번호 : ', 'Serial Number : ');
    CapWriteError: TLanguageString = ('지우기 에러 (중요) : ', 'Erase Errors (Important) : ');
    CapReadError: TLanguageString = ('읽기 에러 (중요) : ', 'Read Errors (Important) : ');
    CapRepSect: TLanguageString = ('치환된 섹터 (중요) : ', 'Reallocated Sectors (Important) : ');
    CapAlign: TLanguageString = ('파티션 배열 (중요) : ', 'Partition Align (Important) : ');
    CapStatus: TLanguageString = ('상태 : ', 'Status : ');
    CapUnknown: TLanguageString = ('알 수 없음', 'Unknown');
    CapConnSpeed: Array[0..3] of String = ('SATA 1.5Gb/s (', 'SATA 3.0Gb/s (', 'SATA 6.0Gb/s (', 'USB');
    CapSupportNCQ: TLanguageString = ('AHCI 사용', 'AHCI Working');
    CapNonSupNCQ: TLanguageString = ('AHCI 꺼짐', 'AHCI Off');


    CapSafe: TLanguageString = ('정상입니다.', 'Safe.');
    CapNotSafeRepSect: TLanguageString = ('SSD의 수명이 끝나가니, SSD 내의 자료 백업을 권고합니다.', 'There are some errors. PLEASE DO BACKUP!');
    CapNotSafeCritical: TLanguageString = ('SSD의 수명이 끝나가니, SSD 내의 자료 백업을 강력히 권고합니다.', 'SERIOUS ERRORS! PLEASE DO BACKUP!');
    CapBadPartition: TLanguageString = ('파티션이 어긋났으니, 윈도 7에서 파티션을 다시 나눠주세요.', 'Partition align error. Re-partition with Win 7 setup tool.');
    CapOldVersion: TLanguageString = (' (구버전)', ' (Old Version)');

    CriticalWarning: TLanguageString =
      ('치명적 경고 (중요) : ', 'Critical Warning (Important) : ');
    SafeWithoutDot: TLanguageString = ('정상', 'Safe');
    Bad: TLanguageString = ('나쁨', 'BAD');

    CapGood: TLanguageString = (' (정상) ', ' (Good) ');
    CapBad: TLanguageString = ('K - 어긋남) ', 'K - Bad) ');

  //메인 제목줄
    CapToSeeSerial: TLanguageString = (' (시리얼 번호 보려면 X표시 클릭)', ' (To see the serial number, click the ''X'' sign)');
    CapSSDSelOpn: TLanguageString = ('SSD 선택 ▼', 'Select SSD ▼');
    CapSSDSelCls: TLanguageString = ('SSD 선택 ▲', 'Select SSD ▲');


  //메인 버튼
    BtFirmware: TLanguageString = ('펌웨어', 'Firmware');
    BtErase: TLanguageString = ('초기화', 'Erase');
    BtOpt: TLanguageString = ('최적화', 'Optimization');
    BtHelp: TLanguageString = ('도움말 (F1)', 'Help (F1)');
    BtAnaly: TLanguageString = ('사용 통계', 'Statistics');
    BtLifeAnaly: TLanguageString = ('수명 통계', 'Statistics');
    BtTrim: TLanguageString = ('수동 트림', 'Manual Trim');


  //펌웨어 업데이트
    CapFirm: TLanguageString = ('펌웨어 업데이트 USB 만들기', 'Firmware Update using USB flash drive');
    CapSelUSB: TLanguageString = ('USB 선택 : ', 'USB Drive : ');
    CapNewFirm: TLanguageString = ('새 펌웨어 버전 : ', 'Latest firmware : ');
    CapWarnErase: TLanguageString = (' 부팅을 위해, 선택한 USB 내의 모든 자료가 삭제되는 것에 동의합니다.', ' All data in the drive will be ERASED.');
    BtDoUpdate: TLanguageString = ('펌웨어 업데이트 USB 제작 시작하기', 'Start making a F/W Update USB Drive');
    CapCurrFirm: TLanguageString = ('(현재 펌웨어와 동일)', '(Current Firmware)');

    AlrtNoInternet: TLanguageString = ('인터넷에 연결이 되어있지 않습니다.' + Chr(13) + Chr(10) + '연결 후 다시 진행해 주세요.',
                                          'No internet connectivity detected.');
    AlrtNoServer: TLanguageString = ('대상 서버로 연결할 수 없습니다.',
                                          'No response from the destination server.');
    AlrtStartFormat: TLanguageString = ('USB 포맷을 시작합니다. 잠시만 기다려주세요.', 'Start formatting. Wait please.');
    AlrtStartRecord: TLanguageString = ('USB에 펌웨어 업데이터 기록을 시작합니다.' + Chr(13) + Chr(10) + '창이 꺼질때까지 기다려주세요.'
                                              , 'Start writing firmware update. Please wait.');
    AlrtFirmEnd: TLanguageString = ('이제 재시작해서 펌웨어 업데이트를 시작해주세요.', 'Restart and Start updating.');


    CapFirmDwld: TLanguageString = ('펌웨어 다운로드', 'Downloading Firmware');

    AlrtFirmStart: TLanguageString = ('펌웨어 다운로드를 시작합니다.', 'Starting download Firmware.');
    AlrtFirmCanc: TLanguageString = ('펌웨어 다운로드가 취소되었습니다.', 'Download has been canceled.');
    AlrtFirmFail: TLanguageString = ('펌웨어 다운로드가 실패하였습니다.', 'Download has been failed.');


  //초기화
    CapErase: TLanguageString = ('초기화 USB 만들기', 'Secure Erase using USB flash drive');
    CapEraDwld: TLanguageString = ('SSD 초기화 툴 다운로드', 'Downloading Secure Erase tool');
    BtDoErase: TLanguageString = ('초기화 USB 제작 시작하기', 'Start making a Secure Erase USB Drive');

    AlrtEraEnd: TLanguageString = ('이제 재시작해서 SSD 초기화를 시작해주세요.', 'Restart and Start erasing.');


  //초기화 다운로드
    AlrtBootFail: TLanguageString = ('선택하신 초기화 도구에 해당하는 플러그인을 설치하고 다시 시도해주세요.', 'Please download and install suitable plugin pack.');


  //최적화
    CapNameOpt: TLanguageString = ('SSD 최적화', 'SSD Optimization');

    BtDoOpt: TLanguageString = ('최적화 시작하기', 'Start Optimization');
    BtRollback: TLanguageString = ('최적화 되돌리기', 'Rollback');

    CapAlreadyCompleted: TLanguageString = (' (이미 완료됨)', ' (Already Completed)');

    CapOptHiber: TLanguageString = ('하이버네이션 끄기', 'Disable Hibernation');
    CapOptLastAccess: TLanguageString = ('파일 접근 기록 없애기', 'Disable Last access time attribute');
    CapOptSupFetch: TLanguageString = ('프리패치/슈퍼패치 끄기', 'Disable Prefetch/Superfetch Service');

    CapOptPrefetch: TLanguageString = ('프리패치 끄기', 'Disable Prefetch Service');
    CapOptDfrg: TLanguageString = ('부팅/유휴시간 조각 모음 끄기', 'Disable system defragger');
    CapOptIndex: TLanguageString = ('색인 끄기(여분 용량 & 성능 증가 / 파일 찾기 느려짐)', 'Disable Indexing Service');
    CapOptRes: TLanguageString = ('시스템 보호 끄기(여분 용량 & 성능 증가 / 시스템 복구 불가)', 'Disable System Restore Service');

    AlrtOptCmpl: TLanguageString = ('최적화가 완료되었습니다.', 'Optimization has completed.');
    AlrtOptRetCmpl: TLanguageString = ('최적화 복구가 완료되었습니다.', 'Rollback operation has completed.');


  //수동 트림
    CapTrimName: TLanguageString = ('수동 트림 (백업을 강력히 권고합니다)', 'Manual Trim (Use at your own risk)');
    CapStartManTrim: TLanguageString = ('수동 트림 시작하기', 'Start manual trim');
    BtSemiAutoTrim: TLanguageString = ('반자동 트림 설정', 'Semi-auto trim');
    CapLocalDisk: TLanguageString = ('로컬 디스크', 'Local Disk');
    CapRemvDisk: TLanguageString = ('이동식 디스크', 'Removable Disk');
    CapProg1: TLanguageString = ('진행 상황 : ', 'Progress : ');
    CapProg2: TLanguageString = ('트림 도중 자료 유실 우려가 있으니, 쓰기 작업 병행을 최소화해주세요', 'DO NOT DO WRITE-INTENSIVE OPERATION DURING MANUAL TRIM.');
    CapProg3: TLanguageString = ('드라이브 트림 진행중', 'Drive trimming');


  //반자동 트림
    CapSemiAutoTrim: TLanguageString = ('반자동 트림', 'Semi-Auto Trim (Use at your own risk)');
    CapSemiAutoTrimExp: TLanguageString = ('반자동 트림이란 1분 이상 유휴 상태 지속시 트림을 수행하는 기능입니다.',
                                                 'Semi-auto trim is the trim setting that does trim after 60 secs of idle time.');
    ChkSemiAutoTrim: TLanguageString = ('이 드라이브에 대해서 반자동 트림 적용', 'Apply semi-auto trim');
    BtRtn: TLanguageString = ('돌아가기', 'Return');

  //반자동 트림
    CapAppDisk: TLanguageString = ('적용될 드라이브 : ', 'Drives to be applied : ');

  //사용 통계
    CapAnaly: TLanguageString = ('SSD 사용 통계', 'SSD Statistics');
    CapLifeAnaly: TLanguageString = ('SSD 수명 통계', 'SSD Statistics');
    CapNandWrite: TLanguageString = ('낸드 쓰기 : ', 'Nand Writes : ');
    CapHostWrite: TLanguageString = ('호스트 쓰기 : ', 'Host Writes : ');
    CapMylesLeft: TLanguageString = ('남은 예비 공간 : ', 'Available Reserved Space : ');
    CapSSDLifeLeft: TLanguageString = ('남은 수명 : ', 'SSD Life : ');
    CapWearLevel: TLanguageString = ('웨어 레벨링 횟수 : ', 'Wear Leveling Count : ');
    CapCannotTrust: TLanguageString = (' (신뢰할 수 없음)', ' (DO NOT TRUST)');

    CapAvg: Array[0..2] of TLanguageString = (('최근 30일 평균 : ', 'Average write per day (Recent 30 days) : '),
                                                    ('최근 3개월 평균 : ' , 'Average write per day (Recent 3 months) : '),
                                                    ('최근 6개월 평균 : ' , 'Average write per day (Recent 6 months) : '));
    CaprAvg: Array[0..2] of TLanguageString = (('최근 30일 평균 : ', 'Average replaced sectors per day (Recent 30 days) : '),
                                                     ('최근 3개월 평균 : ' , 'Average replaced sectors per day (Recent 3 months) : '),
                                                     ('최근 6개월 평균 : ' , 'Average replaced sectors per day (Recent 6 months) : '));
    CapToday: TLanguageString = ('오늘 사용량 : ' , 'Today Usage : ');
    CaprToday: TLanguageString = ('오늘 증가한 치환 섹터 : ' , 'Replaced Sectors (Today) : ');
    CapPowerTime: TLanguageString = ('전원이 들어온 시간 : ', 'Total Power-On Hours : ');

    CapDay: TLanguageString = ('일' , 'day');
    CapCount: TLanguageString = ('개' , '');
    CapSec: TLanguageString = ('초' , 'sec');
    CapMin: TLanguageString = ('분' , 'min');
    CapHour: TLanguageString = ('시간' , 'hour');
    CapMultiple: TLanguageString = ('' , 's');

    CapPortable: TLanguageString = ('포터블 버전에서는 지원하지 않습니다.', 'Portable edition doesn''t have this feature.');
    CapNotSupported: TLanguageString = ('미지원 제품입니다', 'Not supported');


  //판올림
    CapCurrVer: TLanguageString = ('지금의 판 : ' , 'Current version : ');
    CapNewVer: TLanguageString = ('새로운 판 : ' , 'Latest version : ');
    CapUpdQues: TLanguageString = ('지금 판올림을 하시겠습니까?' , 'Would you like to get update?');
    CapUpdDwld: TLanguageString = ('판올림 다운로드', 'Downloading update');
    BtDnldCncl: TLanguageString = ('판올림 취소', 'Cancel update');

    AlrtUpdateExit: TLanguageString = ('본 프로그램의 판올림을 위해 프로그램을 종료합니다.', 'Closing program to do update.');
    AlrtVerCanc: TLanguageString = ('판올림 다운로드가 취소되었습니다.', 'Update download has been canceled.');
    AlrtWrongCodesign: TLanguageString = ('판올림이 잘못된 코드사인을 포함하고 있습니다.' + Chr(13) + Chr(10) + '개발자에게 알려주세요.', 'Update download has wrong codesign.' + Chr(13) + Chr(10) + 'Please report it to developer.');
    AlrtVerTraff: TLanguageString = ('트래픽이 초과되었으므로, 직접 다운받아주세요.' + Chr(13) + Chr(10) + '죄송합니다.',
                                            'The update server is over its quota. Try later.' + Chr(13) + Chr(10) + 'Sorry for inconvenience.');
    AlrtNewVer: TLanguageString = ('새로운 판 발견' , 'Update available');


  //수명 감시 서비스
    CapWrongBuf: TLanguageString = ('쓰기 캐시 설정 잘못됨' , 'has wrong setting');
    CapWrongBuf2: TLanguageString = ('비 샌드포스 SSD에서 절대 쓰기 캐시를 끄지 마세요' , 'Do NOT turn off write cache on non-sandforce SSDs.');

    CapBck: TLanguageString = ('즉시 백업 요망' , 'requires BACKUP IMMEDIATELY');
    CapBckRepSector: TLanguageString = ('SSD에서 죽은 섹터' , 'It has dead sector');
    CapBckREError: TLanguageString = ('SSD에서 죽은 섹터' , 'It has dead sector');
    CapOcc: TLanguageString = ('발생' , '');


  //에러 메시지
    AlrtNoFirmware: TLanguageString = ('펌웨어 파일이 없습니다. 프로그램을 재설치해주세요.', 'There isn''t any Firmware File. Please re-install this tool.');
    AlrtOSError: TLanguageString = ('이 운영체제는 지원하지 않는 운영체제(XP 미만)이므로 종료합니다.', 'This tools doesn''t support OS under Windows XP.');
    AlrtNoSupport: TLanguageString = ('나래온 툴 지원 SSD가 검출되지 않았습니다.', 'There is no supported SSD.');
    AlrtNoUSB: TLanguageString = ('USB 저장장치를 연결해주세요.', 'Please connect a USB Flash Drive');
    AlrtNoUpdate: TLanguageString = ('이미 최신 버전입니다.', 'Already your SSD has the latest firmware.');
    AlrtNoFirmSupport: TLanguageString = ('해당 SSD의 펌웨어 업데이트는 지원하지 않습니다.', 'This SSD cannot be updated by Naraeon SSD Tools.');
    AlrtNoCheck: TLanguageString = ('USB 자료 손실 동의 체크박스에 체크해 주세요.', 'Read and Check the caution.');
    AlrtAutoCopy: TLanguageString = ('- 이 내용은 자동으로 복사됩니다 -', '- This message has copied -');


  //다운로드
    CapTime: TLanguageString = ('남은 시간 : ', 'Remaining time : ');

  //나래온 넷 주소
    AddrSecureErase: TLanguageString = ('http://naraeon.net/plugin/plugin_kor.htm', 'http://naraeon.net/plugin/plugin_eng.htm');
    AddrUpdChk: TLanguageString = ('http://nstupdate.naraeon.net/latestSSDTools.htm', 'http://nstupdate.naraeon.net/latestSSDTools_eng.htm');

  //진단
    DiagName: TLanguageString = ('나래온 툴 진단도구', 'NSTools Diagnosis');
    DiagContents: TLanguageString = ('진단에 필요한 정보가 복사되었습니다.' + Chr(13) + Chr(10) + '필요한 곳에 Ctrl + V로 붙여넣어 사용하세요.',
                                            'Information for Diagnosis copied.' + Chr(13) + Chr(10) + 'Press Ctrl + V when needed.');

procedure DetermineLanguage;

implementation

procedure DetermineLanguage;
begin
  if GetSystemDefaultLangID = 1042 then
    CurrLang := LANG_HANGUL
  else
    CurrLang := LANG_ENGLISH;
end;
end.
