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

## Announcement #
* 숙제 4의 결과를 [여기](https://docs.google.com/spreadsheets/d/1J0w3gUf_1PQ66Gbj5EzDioLTFdnxlBIscW1CXRWG0-k/edit?usp=sharing)서 확인하세요.
  + 혹시 지금까지 메일로 보낸 문의에 답변이 오지 않았거나, 과제 관련 클레임이 반영되지 않은 경우 다시 메일을 주세요.
* Project의 [뼈대코드](homeworks/proj-skeleton)는 12/02(수) 오후 10시 40분에 업로드한 것이 안정된 버전입니다. [proj-instruction](homeworks/proj-skeleton/proj_instr.md)를 꼭 읽고 진행해주세요.
* 11/30(월)은 휴강, 12/2(수)는 기존과 같이 실습을 진행합니다.
* 숙제 3-2 여섯 분의 점수가 갱신되었습니다. 채점용 library에 버그가 있었습니다.
  + 2009-11604, 2010-11162, 2011-11749, 2012-10097, 2014-16477, 2014-18935
* 숙제 7,8의 뼈대코드가 공개되었습니다. 각각의 instr을 잘 읽고 시작해주세요.
* 6-5 새 자가채점기 오타가 수정되었습니다. (11월 20일 정오)
* 과제 6-1, 6-7 채점기 버그 수정 및 make 사용자를 위한 채점기용 makefile을 추가하였습니다. - (seojangho님 및 hyunmin-personal님의 기여)
* 교수님 강의는 11/23, 11/25, 12/14, 12/16 4번 진행할 예정입니다.
* 숙제 1, 2의 [모범답안](homeworks/)을 만들어보았습니다.
  + 채점기는 모두 통과합니다. 다만 버그가 있을 가능성도 있습니다.
  + 늦게 제출할 경우, 이 코드를 참고는 하실 수 있지만 구조를 직접 짜시지 않으면 저희의 code clone detector에 걸릴 가능성이 높습니다.
* 수업/실습/과제 관련 질문의 경우 이 GitHub 페이지의 issues 게시판을 이용할 것입니다.
 + GitHub에 계정이 없으신 수강생은 지금 만들어두시면 앞으로 도움이 될 것입니다. 
 + 답변은 조교 또는 학생 누구든 참여하시기를 권장합니다.

## 출석 ##
* 출석 체크 자료가 올라왔습니다. [이 곳](attendance/instruction.md)에서 정보를 확인하세요.
 + 출석 체크용 사진 촬영은 쉬는 시간이 끝난 다음 5분 후, 또는 17:30에 이루어집니다.
 + 9/23(수) 수업부터는 출석 체크 시간에 자리를 비울 경우 출석을 인정해드리지 않을 예정이니 주의하시기 바랍니다.
 + 현재 11/25(수)까지의 출석 체크 사진이 업로드되어 있습니다.
 + 출석 체크 중복 사례와 관련하여 이슈를 등록하였습니다. 본인의 번호에 다른 학번이 입력되었다 생각되시면 이슈를 통해 조율하시기 바랍니다.

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
