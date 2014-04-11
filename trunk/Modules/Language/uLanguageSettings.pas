unit uLanguageSettings;

interface

var
  CurrLang: Integer;

const
  LANG_HANGUL = 0;
  LANG_ENGLISH = 1;

const
  //캐시 오류
    VistaFont: Array[0..1] of String = ('맑은 고딕', 'Segoe UI');
    XPFont: Array[0..1] of String = ('돋움', 'Verdana');

  //캐시 오류
    ErrCache: Array[0..1] of String = ('다음 장치에서 잘못된 캐시 설정이 나타나 수정했습니다', 'Wrong cache setting');

  //메인
    CapConnState: Array[0..1] of String = ('연결 상태 : ', 'Connected State : ');
    CapFirmware: Array[0..1] of String = ('펌웨어 버전 : ', 'Firmware : ');
    CapSerial: Array[0..1] of String = ('시리얼 번호 : ', 'Serial Number : ');
    CapWriteError: Array[0..1] of String = ('지우기 에러 (중요) : ', 'Erase Errors (Important) : ');
    CapReadError: Array[0..1] of String = ('읽기 에러 : ', 'Read Errors: ');
    CapRepSect: Array[0..1] of String = ('치환된 섹터 (중요) : ', 'Reallocated Sectors (Important) : ');
    CapAlign: Array[0..1] of String = ('파티션 배열 (중요) : ', 'Partition Align (Important) : ');
    CapNotSafe: Array[0..1] of String = ('상태 : ', 'Status : ');
    CapUnknown: Array[0..1] of String = ('알 수 없음', 'Unknown');
    ConnState: Array[0..3] of String = ('SATA 1.5Gb/s (', 'SATA 3.0Gb/s (', 'SATA 6.0Gb/s (', 'USB');
    CapSupportNCQ: Array[0..1] of String = ('AHCI 사용', 'AHCI Working');
    CapNonSupNCQ: Array[0..1] of String = ('AHCI 꺼짐', 'AHCI Off');


    CapSafe: Array[0..1] of String = ('정상입니다.', 'Safe.');
    CapNotSafeRepSect: Array[0..1] of String = ('SSD의 수명이 끝나가니, SSD 내의 자료 백업을 강력히 권고합니다.', 'There are some errors. PLEASE DO BACKUP!');
    CapNotSafeEraseErrors: Array[0..1] of String = ('SSD의 수명이 끝나가니, SSD 내의 자료 백업을 강력히 권고합니다.', 'SERIOUS ERRORS! PLEASE DO BACKUP!');
    CapBadPartition: Array[0..1] of String = ('파티션이 어긋났으니, 윈도 7에서 파티션을 다시 나눠주세요.', 'Partition align error. Re-partition with Win 7 setup tool.');
    CapOldVersion: Array[0..1] of String = (' (구버전)', ' (Old Version)');

    CapGood: Array[0..1] of String = (' (정상) ', ' (Good) ');
    CapBad: Array[0..1] of String = ('K - 어긋남) ', 'K - Bad) ');

  //메인 제목줄
    CapToSeeSerial: Array[0..1] of String = (' (시리얼 번호 보려면 X표시 클릭)', ' (To see the serial number, click the ''X'' sign)');
    CapSSDSelOpn: Array[0..1] of String = ('SSD 선택 ▼', 'Select SSD ▼');
    CapSSDSelCls: Array[0..1] of String = ('SSD 선택 ▲', 'Select SSD ▲');


  //메인 버튼
    BtFirmware: Array[0..1] of String = ('펌웨어', 'Firmware');
    BtErase: Array[0..1] of String = ('초기화', 'Erase');
    BtOpt: Array[0..1] of String = ('최적화', 'Optimization');
    BtHelp: Array[0..1] of String = ('도움말 (F1)', 'Help (F1)');
    BtAnaly: Array[0..1] of String = ('사용 통계', 'Statistics');
    BtLifeAnaly: Array[0..1] of String = ('수명 통계', 'Statistics');
    BtTrim: Array[0..1] of String = ('수동 트림', 'Manual Trim');


  //펌웨어 업데이트
    CapFirm: Array[0..1] of String = ('펌웨어 업데이트 USB 만들기', 'Firmware Update using USB flash drive');
    CapSelUSB: Array[0..1] of String = ('USB 선택 : ', 'USB Drive : ');
    CapNewFirm: Array[0..1] of String = ('새 펌웨어 버전 : ', 'Latest firmware : ');
    CapWarnErase: Array[0..1] of String = (' 부팅을 위해, 선택한 USB 내의 모든 자료가 삭제되는 것에 동의합니다.', ' All data in the drive will be ERASED.');
    BtDoUpdate: Array[0..1] of String = ('펌웨어 업데이트 USB 제작 시작하기', 'Start making a F/W Update USB Drive');
    CapCurrFirm: Array[0..1] of String = ('(현재 펌웨어와 동일)', '(Current Firmware)');

    AlrtNoInternet: Array[0..1] of String = ('인터넷에 연결이 되어있지 않습니다.' + Chr(13) + Chr(10) + '연결 후 다시 진행해 주세요.',
                                          'No internet connectivity detected.');
    AlrtNoServer: Array[0..1] of String = ('대상 서버로 연결할 수 없습니다.',
                                          'No response from the destination server.');
    AlrtStartFormat: Array[0..1] of String = ('USB 포맷을 시작합니다. 잠시만 기다려주세요.', 'Start formatting. Wait please.');
    AlrtStartRecord: Array[0..1] of String = ('USB에 펌웨어 업데이터 기록을 시작합니다.' + Chr(13) + Chr(10) + '창이 꺼질때까지 기다려주세요.'
                                              , 'Start writing firmware update. Please wait.');
    AlrtFirmEnd: Array[0..1] of String = ('이제 재시작해서 펌웨어 업데이트를 시작해주세요.', 'Restart and Start updating.');


    CapFirmDwld: Array[0..1] of String = ('펌웨어 다운로드', 'Downloading Firmware');
    CapBootInDwld: Array[0..1] of String = ('부팅용 USB 제작기 다운로드', 'Downloading bootable thumb drive maker');

    AlrtFirmStart: Array[0..1] of String = ('펌웨어 다운로드를 시작합니다.', 'Starting download Firmware.');
    AlrtBootInStart: Array[0..1] of String = ('부팅용 USB 제작기 다운로드를 시작합니다.', 'Starting download bootable thumb drive maker.');
    AlrtFirmCanc: Array[0..1] of String = ('펌웨어 다운로드가 취소되었습니다.', 'Download has been canceled.');
    AlrtFirmFail: Array[0..1] of String = ('펌웨어 다운로드가 실패하였습니다.', 'Download has been failed.');


  //초기화
    CapErase: Array[0..1] of String = ('초기화 USB 만들기', 'Secure Erase using USB flash drive');
    CapEraDwld: Array[0..1] of String = ('SSD 초기화 툴 다운로드', 'Downloading Secure Erase tool');
    BtDoErase: Array[0..1] of String = ('초기화 USB 제작 시작하기', 'Start making a Secure Erase USB Drive');

    AlrtEraEnd: Array[0..1] of String = ('이제 재시작해서 SSD 초기화를 시작해주세요.', 'Restart and Start erasing.');


  //초기화 다운로드
    AlrtBootFail: Array[0..1] of String = ('선택하신 초기화 도구에 해당하는 플러그인을 설치하고 다시 시도해주세요.', 'Please download and install suitable plugin pack.');


  //최적화
    CapNameOpt: Array[0..1] of String = ('SSD 최적화', 'SSD Optimization');

    BtDoOpt: Array[0..1] of String = ('최적화 시작하기', 'Start Optimization');
    BtRollback: Array[0..1] of String = ('최적화 되돌리기', 'Rollback');

    CapAlreadyCompleted: Array[0..1] of String = (' (이미 완료됨)', ' (Already Completed)');

    CapOptHiber: Array[0..1] of String = ('하이버네이션 끄기', 'Disable Hibernation');
    CapOptFilerec: Array[0..1] of String = ('파일 접근 기록 없애기', 'Disable Last access time attribute');
    CapOptSupFetch: Array[0..1] of String = ('프리패치/슈퍼패치 끄기', 'Disable Prefetch/Superfetch Service');

    CapOptPrefetch: Array[0..1] of String = ('프리패치 끄기', 'Disable Prefetch Service');
    CapOptDfrg: Array[0..1] of String = ('부팅/유휴시간 조각 모음 끄기', 'Disable system defragger');
    CapOptIndex: Array[0..1] of String = ('색인 끄기(여분 용량 & 성능 증가 / 파일 찾기 느려짐)', 'Disable Indexing Service');
    CapOptRes: Array[0..1] of String = ('시스템 보호 끄기(여분 용량 & 성능 증가 / 시스템 복구 불가)', 'Disable System Restore Service');

    AlrtOptCmpl: Array[0..1] of String = ('최적화가 완료되었습니다.', 'Optimization has completed.');
    AlrtOptRetCmpl: Array[0..1] of String = ('최적화 복구가 완료되었습니다.', 'Rollback operation has completed.');


  //수동 트림
    CapTrimName: Array[0..1] of String = ('수동 트림 (백업을 강력히 권고합니다)', 'Manual Trim (Use at your own risk)');
    CapStartManTrim: Array[0..1] of String = ('수동 트림 시작하기', 'Start manual trim');
    BtSemiAutoTrim: Array[0..1] of String = ('반자동 트림 설정', 'Semi-auto trim');
    CapLocalDisk: Array[0..1] of String = ('로컬 디스크', 'Local Disk');
    CapRemvDisk: Array[0..1] of String = ('이동식 디스크', 'Removable Disk');
    CapProg1: Array[0..1] of String = ('진행 상황 : ', 'Progress : ');
    CapProg2: Array[0..1] of String = ('드라이브 트림 진행중', 'Drive trimming');
    CapProg3: Array[0..1] of String = ('트림 도중 자료 유실 우려가 있으니, 쓰기 작업 병행을 최소화해주세요', 'DO NOT DO WRITE-INTENSIVE OPERATION DURING MANUAL TRIM.');


  //반자동 트림
    CapSemiAutoTrim: Array[0..1] of String = ('반자동 트림', 'Semi-Auto Trim (Use at your own risk)');
    CapSemiAutoTrimExp: Array[0..1] of String = ('반자동 트림이란 1분 이상 유휴 상태 지속시 트림을 수행하는 기능입니다.',
                                                 'Semi-auto trim is the trim setting that does trim after 60 secs of idle time.');
    ChkSemiAutoTrim: Array[0..1] of String = ('이 드라이브에 대해서 반자동 트림 적용', 'Apply semi-auto trim');
    BtRtn: Array[0..1] of String = ('돌아가기', 'Return');

  //반자동 트림
    CapAppDisk: Array[0..1] of String = ('적용될 드라이브 : ', 'Drives to be applied : ');

  //사용 통계
    CapAnaly: Array[0..1] of String = ('SSD 사용 통계', 'SSD Statistics');
    CapLifeAnaly: Array[0..1] of String = ('SSD 수명 통계', 'SSD Statistics');
    CapNandWrite: Array[0..1] of String = ('낸드 쓰기 : ', 'Nand Writes : ');
    CapHostWrite: Array[0..1] of String = ('호스트 쓰기 : ', 'Host Writes : ');
    CapMylesLeft: Array[0..1] of String = ('남은 예비 공간 : ', 'Available Reserved Space : ');
    CapSSDLifeLeft: Array[0..1] of String = ('남은 수명 : ', 'SSD Life : ');
    CapWearLevel: Array[0..1] of String = ('웨어 레벨링 횟수 : ', 'Wear Leveling Count : ');
    CapCannotTrust: Array[0..1] of String = (' (신뢰할 수 없음)', ' (DO NOT TRUST)');

    CapAvg: Array[0..2] of Array[0..1] of String = (('최근 6개월 평균 : ' , 'Average write per day (Recent 6 months) : '),
                                                    ('최근 3개월 평균 : ' , 'Average write per day (Recent 3 months) : '),
                                                    ('최근 30일 평균 : ', 'Average write per day (Recent 30 days) : '));
    CaprAvg: Array[0..2] of Array[0..1] of String = (('최근 6개월 평균 : ' , 'Average replaced sectors per day (Recent 6 months) : '),
                                                     ('최근 3개월 평균 : ' , 'Average replaced sectors per day (Recent 3 months) : '),
                                                     ('최근 30일 평균 : ', 'Average replaced sectors per day (Recent 30 days) : '));
    CapToday: Array[0..1] of String = ('오늘 사용량 : ' , 'Today Usage : ');
    CaprToday: Array[0..1] of String = ('오늘 증가한 치환 섹터 : ' , 'Replaced Sectors (Today) : ');
    CapPowerTime: Array[0..1] of String = ('전원이 들어온 시간 : ', 'Total Power-On Hours : ');

    CapDay: Array[0..1] of String = ('일' , 'day');
    CapCount: Array[0..1] of String = ('개' , '');
    CapSec: Array[0..1] of String = ('초' , 'sec');
    CapMin: Array[0..1] of String = ('분' , 'min');
    CapHour: Array[0..1] of String = ('시간' , 'hrs');

    CapPortable: Array[0..1] of String = ('포터블 버전에서는 지원하지 않습니다.', 'Portable edition doesn''t have this feature.');
    CapNotSupported: Array[0..1] of String = ('미지원 제품입니다', 'Not supported');


  //판올림
    CapCurrVer: Array[0..1] of String = ('지금의 판 : ' , 'Current version : ');
    CapNewVer: Array[0..1] of String = ('새로운 판 : ' , 'Latest version : ');
    CapUpdQues: Array[0..1] of String = ('지금 판올림을 하시겠습니까?' , 'Would you like to get update?');
    CapUpdDwld: Array[0..1] of String = ('판올림 다운로드', 'Downloading update');
    BtDnldCncl: Array[0..1] of String = ('판올림 취소', 'Cancel update');

    AlrtUpdateExit: Array[0..1] of String = ('본 프로그램의 판올림을 위해 프로그램을 종료합니다.', 'Closing program to do update.');
    AlrtVerCanc: Array[0..1] of String = ('판올림 다운로드가 취소되었습니다.', 'Update download has been canceled.');
    AlrtVerTraff: Array[0..1] of String = ('트래픽이 초과되었으므로, 직접 다운받아주세요.' + Chr(13) + Chr(10) + '죄송합니다.',
                                            'The update server is over its quota. Try later.' + Chr(13) + Chr(10) + 'Sorry for inconvenience.');
    AlrtNewVer: Array[0..1] of String = ('새로운 판 발견' , 'Update available');


  //수명 감시 서비스
    CapWrongBuf: Array[0..1] of String = ('쓰기 캐시 설정 잘못됨' , 'has wrong setting');
    CapWrongBuf2: Array[0..1] of String = ('비 샌드포스 SSD에서 절대 쓰기 캐시를 끄지 마세요' , 'Do NOT turn off write cache on non-sandforce SSDs.');

    CapBck: Array[0..1] of String = ('즉시 백업 요망' , 'requires BACKUP IMMEDIATELY');
    CapBck2: Array[0..1] of String = ('SSD에서 죽은 섹터' , 'It has dead sector');
    CapOcc: Array[0..1] of String = ('발생' , '');


  //에러 메시지
    AlrtNoFirmware: Array[0..1] of String = ('펌웨어 파일이 없습니다. 프로그램을 재설치해주세요.', 'There isn''t any Firmware File. Please re-install this tool.');
    AlrtOSError: Array[0..1] of String = ('이 운영체제는 지원하지 않는 운영체제(XP 미만)이므로 종료합니다.', 'This tools doesn''t support OS under Windows XP.');
    AlrtNoSupport: Array[0..1] of String = ('나래온 툴 지원 SSD가 검출되지 않았습니다.', 'There is no supported SSD.');
    AlrtNoUSB: Array[0..1] of String = ('USB 저장장치를 연결해주세요.', 'Please connect a USB Flash Drive');
    AlrtNoUpdate: Array[0..1] of String = ('이미 최신 버전입니다.', 'Already your SSD has the latest firmware.');
    AlrtNoFirmSupport: Array[0..1] of String = ('해당 SSD의 펌웨어 업데이트를 지원하지 않습니다.', 'This SSD cannot be updated by Naraeon SSD Tools.');
    AlrtNoCheck: Array[0..1] of String = ('USB 자료 손실 동의 체크박스에 체크해 주세요.', 'Read and Check the caution.');
    AlrtAutoCopy: Array[0..1] of String = ('- 이 내용은 자동으로 복사됩니다 -', '- This message has copied -');


  //다운로드
    CapSpeed: Array[0..1] of String = ('전송 속도 : ', 'Transfer Speed : ');
    CapTime: Array[0..1] of String = ('남은 시간 : ', 'Remaining time : ');

  //나래온 넷 주소
    AddrSecureErase: Array[0..1] of String = ('http://naraeon.net/plugin/plugin_kor.htm', 'http://naraeon.net/plugin/plugin_eng.htm');
implementation

end.
