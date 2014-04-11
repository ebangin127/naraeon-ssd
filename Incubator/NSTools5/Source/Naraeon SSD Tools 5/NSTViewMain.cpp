
// NSTViewMain.cpp : 구현 파일
//
#include "NSTViewMain.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#endif


// CNSTViewMain 대화 상자




CNSTViewMain::CNSTViewMain(CWnd* pParent /*=NULL*/)
	: CNSTDialog(CNSTViewMain::IDD, pParent)
{
	m_hIcon = AfxGetApp()->LoadIcon(IDR_MAINFRAME);
}

void CNSTViewMain::DoDataExchange(CDataExchange* pDX)
{
	CDialogEx::DoDataExchange(pDX);
	DDX_Control(pDX, IDC_ILOGO, iLogo);
}

BEGIN_MESSAGE_MAP(CNSTViewMain, CDialogEx)
	ON_WM_PAINT()
	ON_WM_QUERYDRAGICON()
	//ON_BN_CLICKED(ID_TEST, &CNSTViewMain::OnBnClickedTest)
END_MESSAGE_MAP()


// CNSTViewMain 메시지 처리기

BOOL CNSTViewMain::OnInitDialog()
{
	COLORREF BgColor = RGB(255,255,255);
	CDialogEx::OnInitDialog();

	// 이 대화 상자의 아이콘을 설정합니다. 응용 프로그램의 주 창이 대화 상자가 아닐 경우에는
	//  프레임워크가 이 작업을 자동으로 수행합니다.
	SetIcon(m_hIcon, TRUE);			// 큰 아이콘을 설정합니다.
	SetIcon(m_hIcon, FALSE);		// 작은 아이콘을 설정합니다.

	// TODO: 여기에 추가 초기화 작업을 추가합니다.
	CWnd* ILOGOITEM = GetDlgItem(IDC_ILOGO);

	CImage graphics;
	graphics.Load(L"c:\\logo.png");
	iLogo.SetBitmap(graphics);
	SetBgColor( RGB(255,255,255));

	return TRUE;  // 포커스를 컨트롤에 설정하지 않으면 TRUE를 반환합니다.
}

// 대화 상자에 최소화 단추를 추가할 경우 아이콘을 그리려면
//  아래 코드가 필요합니다. 문서/뷰 모델을 사용하는 MFC 응용 프로그램의 경우에는
//  프레임워크에서 이 작업을 자동으로 수행합니다.

void CNSTViewMain::OnPaint()
{
	if (IsIconic())
	{
		CPaintDC dc(this); // 그리기를 위한 디바이스 컨텍스트입니다.

		SendMessage(WM_ICONERASEBKGND, reinterpret_cast<WPARAM>(dc.GetSafeHdc()), 0);

		// 클라이언트 사각형에서 아이콘을 가운데에 맞춥니다.
		int cxIcon = GetSystemMetrics(SM_CXICON);
		int cyIcon = GetSystemMetrics(SM_CYICON);
		CRect rect;
		GetClientRect(&rect);
		int x = (rect.Width() - cxIcon + 1) / 2;
		int y = (rect.Height() - cyIcon + 1) / 2;

		// 아이콘을 그립니다.
		dc.DrawIcon(x, y, m_hIcon);
	}
	else
	{
		CDialogEx::OnPaint();
	}
}

// 사용자가 최소화된 창을 끄는 동안에 커서가 표시되도록 시스템에서
//  이 함수를 호출합니다.
HCURSOR CNSTViewMain::OnQueryDragIcon()
{
	return static_cast<HCURSOR>(m_hIcon);
}
