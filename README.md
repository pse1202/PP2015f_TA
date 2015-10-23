# TA Page - 4190.210 프로그래밍 원리 2015 Fall @ SNU #

Instructor: [허충길 교수님](http://sf.snu.ac.kr/gil.hur/)

TA: [김윤승](http://sf.snu.ac.kr/yoonseung.kim/),
[박상훈](http://sf.snu.ac.kr/sanghoon.park/)
    ([Software Foundations Laboratory](http://sf.snu.ac.kr/)
    / [CSE](http://cse.snu.ac.kr)
    / [Seoul National University](http://www.snu.ac.kr))
    
TA Email: (pp2015fall at sf dot snu dot ac dot kr)
* 수업 관련 질문은 [issues](https://github.com/snu-sf-class/PP2015f_TA/issues)를 이용해주세요.
* 이외의 개인적인 일은 조교메일 - 조교 개인 메일 말고 위의 공식 조교 메일 pp2015fall - 을 이용해주세요!

## Times ##

* 수업: Mon 16:00-17:50 @ Bldg 301 Rm 118
* 실습: Wed 16:00-17:50 @ Bldg 302 Rm 311-1
* 조교 Office Hour: Mon, Wed 15:20-16:00 @ Bldg 301 Rm 554-1 혹은 메일을 통한 개별약속.
  + 개인 사정상 부득이하게 자리를 비울 수 있으므로 미리 연락 주시는 것이 좋습니다.

## Links ##

* [Course Page](http://sf.snu.ac.kr/gil.hur/4190.210/15/)
* [Issues Board](https://github.com/snu-sf-class/PP2015f_TA/issues)
* [Lab Materials](/lab)

## Announcement ##
* 3-3 자가채점기가 업로드되었습니다. 여러 번 테스트를 거쳤지만 문제가 발견될 경우 알려주십시오.
* 1-7 재채점 희망자 조사합니다.
  + 1-7의 문제 조건은 다른 사람이 구현한 1-6에 대해서도 잘 작동해야 하는 것이었지만, 그 조건이 문서에 제대로 명시되어있지 않다는 의견이 있었습니다. 따라서 본인이 구현한 1-6을 사용해 재채점을 하기를 희망하는 분이 있으면 조교 메일로 연락을 주십시오.
  + 다음 주 월요일(10/26)까지 취합하여 결정하겠습니다.
  + 점수 분포를 본 결과 수요가 많지 않을 것 같아 효율을 위해 개별적으로 조사를 합니다.
* 숙제 1의 결과를 [여기](https://docs.google.com/spreadsheets/d/1J0w3gUf_1PQ66Gbj5EzDioLTFdnxlBIscW1CXRWG0-k/edit?usp=sharing)서 확인하세요.
  + 이 점수는 테스트 케이스를 이용한 채점만 된 상태인데, 드랍 기간 때문에 점수를 확인하길 원하는 학생이 있어 미리 공지합니다.
  + 코드를 확인하여 문제 조건을 지키지 않은 경우는 추후 감점될 수 있습니다.
  + 부정행위 적발(타인 코드 복사)은 학기 말에 합니다.
  + hw1-raw 탭에서 테스트 실행 결과를 보실 수 있고, 취합된 점수는 hw1 탭에 있습니다.
  + 붉은 색으로 칠해진 점수는 실제 프로그램 내용 이외의, 포맷 등의 문제로 인해 채점시 오류가 발생한 경우입니다. 수정하여 채점하였으나, 이후로는 주의해주시길 바랍니다.
* 숙제 3-3의 채점기준을 공지합니다. [클릭](homeworks/hw3-skeleton/hw3_instr.md)
* 이번주(10/19, 10/21)도 실습과 강의가 변경됩니다. 수요일 강의실: 302-409
* [숙제3](http://sf.snu.ac.kr/gil.hur/4190.210/15/hws/hw3.pdf)이 나왔습니다. 뼈대코드는 [여기](homeworks/hw3-skeleton)이고, 10/18일 오후 10시 이후에 약간 수정되었습니다. 자세한 내용은 [hw3_instr](homeworks/hw3-skeleton/hw3_instr.md)을 참고하세요. 기한: 10/25(일) 자정
* 지각 제출에 대한 규정이 업데이트 되었습니다. [여기](homeworks/instr-hw.md)를 확인하세요.
* [여기서](https://docs.google.com/spreadsheets/d/1ZNYdz0pdTcGQH9dSImFTuTM_qJxG8KoX3d-V2SwOVbk/edit?usp=sharing) 숙제1, 2 기한 내에 제출받은 파일 목록을 확인하세요.
* 출석 체크 자료가 올라왔습니다. [이 곳](attendance/instruction.md)에서 정보를 확인하세요.
 + 출석 체크용 사진 촬영은 쉬는 시간이 끝난 다음 5분 후, 또는 17:30에 이루어집니다.
 + 9/23(수) 수업부터는 출석 체크 시간에 자리를 비울 경우 출석을 인정해드리지 않을 예정이니 주의하시기 바랍니다.
 + 현재 10/14(수)까지의 출석 체크 사진이 업로드되어 있습니다.
* 수업/실습/과제 관련 질문의 경우 이 GitHub 페이지의 issues 게시판을 이용할 것입니다.
 + GitHub에 계정이 없으신 수강생은 지금 만들어두시면 앞으로 도움이 될 것입니다. 
 + 답변은 조교 또는 학생 누구든 참여하시기를 권장합니다.

## 실습 ##
* 실습은 10/28일부터 한 반으로 운영합니다. (오후 4:00~, 302동 소프트웨어 실습실)
* 실습 자료는 [여기](lab/)에서 확인하실 수 있습니다.

## 숙제 ##
* 하기 전에 [숙제 하는 법](homeworks/instr-hw.md)을 읽으세요. *지각 제출에 관한 규정이 업데이트 되었습니다*
* *중요*: 치팅은 하지 마세요.
  + 우리는 (우수한) clone detector와 지난 10년간의 코드를 가지고 있습니다.
* *중요*: [Racket 6.2](http://download.racket-lang.org)와 [OCaml 4.02](http://ocaml.org/docs/install.html)를 사용하세요. 그렇지 않을 경우 채점이 제대로 이루어지지 않을 수도 있습니다.
* 숙제의 지시를 엄격하게 따라주세요. 특히, 입/출력 포맷을 잘 지켜주세요. 조교가 숙제마다 자가채점기를 배포할 것입니다. 제출 전 꼭 확인해보세요. (그렇지 않을 경우 0점입니다.)

## Reference ##

* Racket(Scheme)
  + [Racket Homepage](http://racket-lang.org)
  + [Download Racket](http://racket-lang.org/download/)
  + [How to Program Racket](http://www.ccs.neu.edu/home/matthias/Style/style/)
* OCaml(ML)
  + [Ocaml tutorial (2013/9/11)](http://ropas.snu.ac.kr/~ta/4190.310/13/ocaml_tutorial13f.pdf)
  + [Ocaml tutorial (2011/9/7)](http://ropas.snu.ac.kr/~ta/4190.310/11f/ocaml_tutorial11f.pdf)
  + [Ocaml tutorial (2011/3/10)](http://ropas.snu.ac.kr/~ta/4190.310/11f/ocaml_tutorial11s.pdf)
  + [Ocaml Homepage](http://caml.inria.fr/)
  + [Download OCaml](http://caml.inria.fr/download.en.html)
  + [The Objective Caml manual](http://caml.inria.fr/pub/docs/manual-ocaml/index.html)
    - [The core library](http://caml.inria.fr/pub/docs/manual-ocaml/manual033.html)
    - [The standard library](http://caml.inria.fr/pub/docs/manual-ocaml/manual034.html)
    - [The Objective Caml langauge reference](http://caml.inria.fr/pub/docs/manual-ocaml/language.html)
  + [Developing Applications With Objective Caml](http://caml.inria.fr/pub/docs/oreilly-book/index.html)
  + [OCaml tutorial](http://ocaml.org/tutorials/)
  + [Books on OCaml](http://ocaml.org/books.html)
  + [Caml programming guidelines](http://caml.inria.fr/resources/doc/guides/guidelines.en.html)
* Version Control System
  + [Git](http://www.git-scm.com)
    - [Git the simple guide](http://rogerdudler.github.io/git-guide/index.html)
    - [Git from the bottom up](https://www.google.co.kr/search?client=safari&rls=en&q=git+from+bottom+up&ie=UTF-8&oe=UTF-8&gws_rd=cr&ei=06ckUqKJGYXAkAX1jYAw):
  + [Mercurial](http://mercurial.selenic.com)
  + [Subversion](http://subversion.tigris.org)
* Editors
  + [Emacs](http://www.gnu.org/s/emacs/)
  + [Vim](http://www.vim.org)
  + [UltraEdit](http://www.ultraedit.com)
