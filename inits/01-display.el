; �б������̤���餻�롣
(show-paren-mode t)

;; ������ʬ�Υϥ��饤��
(transient-mark-mode t)

;; �Դ�
(setq-default line-spacing 0)

;;; Ʊ���Хåե�̾�λ� <2> �Ȥ��ǤϤʤ����ǥ��쥯�ȥ�̾�Ƕ���
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; ��˥塼�С��˥ե�����ѥ���ɽ��
(setq frame-title-format
      (format "%%f - Emacs@%s" (system-name)))

;;�ե���ȥ�å��⡼��
(global-font-lock-mode t)

;;window������
(setq default-frame-alist
      (append (list
               '(width . 175)
               '(height . 47)
               '(top . 0)
               '(left . 0)
               '(alpha . (100 60)))
              default-frame-alist))

;;���̺��粽
(setq ns-use-native-fullscreen nil) ;; native�Υե륹���꡼��Ȥ�ʤ�

;;; tool-bar�Ȥ�ʤ�
(tool-bar-mode 0)

;;����ü�ޤ��褿���ޤ��֤�
(setq truncate-lines nil)
(setq truncate-partial-width-windows nil)

;; �������ȥ��åץ�å���������ɽ��
(setq inhibit-startup-screen t)

;; �������ȥ����򥨥������ꥢ���᤯ɽ������
(setq echo-keystrokes 0.1)

;; ���ֹ桦���ֹ��ɽ��
(line-number-mode 1)
(column-number-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;���Ѷ��򡢥��֡������ζ������Ω������;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defface my-face-tab         '((t (:background "Yellow"))) nil :group 'my-faces)
(defface my-face-zenkaku-spc '((t (:background "LightBlue"))) nil :group 'my-faces)
(defface my-face-spc-at-eol  '((t (:foreground "Red" :underline t))) nil :group 'my-faces)
(defvar my-face-tab         'my-face-tab)
(defvar my-face-zenkaku-spc 'my-face-zenkaku-spc)
(defvar my-face-spc-at-eol  'my-face-spc-at-eol)
(defadvice font-lock-mode (before my-font-lock-mode ())
  (font-lock-add-keywords
   major-mode
   '(("\t" 0 my-face-tab append)
     ("��" 0 my-face-zenkaku-spc append)
     ("[ \t]+$" 0 my-face-spc-at-eol append)
     )))
(ad-enable-advice 'font-lock-mode 'before 'my-font-lock-mode)
(ad-activate 'font-lock-mode)

(font-lock-mode t)
(font-lock-fontify-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; �Խ��Ԥ���Ω������ʸ��߹Ԥ�ϥ��饤��ɽ�������
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defface hlline-face
  '((((class color)
      (background dark))
     (:background "dark slate gray"))
    (((class color)
      (background light))
     (:background  "#E1A9AB"))
    (t
     ()))
  "*Face used by hl-line.")
(setq hl-line-face 'hlline-face)
(global-hl-line-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; �ե�������� ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
(set-face-attribute
 'default nil
 :family "Ricty"
 :height 120)

;;; ��������С��ä�
(set-scroll-bar-mode 'nil)
