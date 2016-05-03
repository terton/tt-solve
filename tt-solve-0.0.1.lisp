;;
;; tt Проект Terton
;; "Решатель" физико-математических задач
;;;
;;
;; Функция solve позволяет решать задачи по физике, набранные на естественном русском языке
;; Лексический анализатор построен таким образом, что выбирает из текста задачи физические величины
;; После которых ищет значения, записанные цифрами
;; Базы функций *funbase* и физических величин *rnk* сформированы вначале, и имеют вид списка
;;
;;

;;; базы, которые представляют собой мозг робота

;; текст задачи
(defvar *task*)

;; (:буква-физ.вел "имя физ.вел" "размерность")
(defvar *rnk* NIL)

;; ((имя функции (переменные...)...)
(defvar *funbase* NIL)

;; 

;;; блок работы с функциями

;; макрос (список лисп) -> (функция лисп)
;; (f (* m g)) -> (defun f (...
 (defmacro list2fun (l)
	   `(defun ,(car l) (&key ,@(list-var l))
		,(cadr l)))

;; вырезает из списка-функции переменные
(defun list-var (l)
	   (set-difference (to-list1 (cdr l)) '(* / + -)))



;; преобразует список списков в список без вложений
;; (()..(())..) -> (..)

(defun to-list1 (lst) (cond
			((null lst) nil )
			((atom (car lst)) (cons (car lst) (to-list1 (cdr lst))))
			( T (append (to-list1 (car lst)) (to-list1 (cdr lst))))))

;; функция-обертка для макроса, позволяющая задать любой список-функция и обработать его макросом
;; для преобразования в лисп-функцию 
(defun formula2fun (l)(list 'list2fun l))

;; добавляет в фазу *funbase* новую функцию по ее виду (есть ее обертка для более удобного использования)
(defun to-funbase (str) (setf *funbase* (cons (list (eval (formula2fun str)) (list-var str)) *funbase*)))

;;; простой лексический анализатор
;; список цифр (и возможно символов) с которых начинается значение физической величины
(defvar *strnum* '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))

;; проверка, является ли первый символ цифрой 
(defun strnum-p (x str) (cond
			       ((null str) nil)
			       ((string= x (car str)) t)
			       (t (strnum-p x (cdr str)))))

;; предикат. проверяет соответствует ли слово нашему условию (первый символ -- цифра)
(defun strnump (x) (let ((str *strnum*)) (strnum-p x str)))

;; две функции простого лексического анализатора find-in-task
;; на входе образец (физ. величина) и строка (задача)
;; на выходе численное зачение физ. величины
(defun cut-task (str n1 &optional n2) (if n1 (subseq str n1 n2) nil))
(defun find-in-task (patt str) (cond
					  ((listp patt) 
					   (cut-task str (position-if #'strnump str)))
					  ((and (stringp patt) (string= " " patt)) 
					   (cut-task str 0 (search patt str)))
					  ((stringp patt) (cut-task str (search patt str)))
					  (T nil)))

;; ищет имя физ.вел в задаче, затем ищет число, заканчивещееся пробелом и выводит его
(defun ph-vol (ph-name) (find-in-task " " (find-in-task *strnum* (find-in-task ph-name *task*))))

;; преобразование в число
(defun strnum2num (str)(with-input-from-string (s str)(read s)))

;;; работа с базами ДНк и РНк
;;; РНк накопленный код (память робота) в котором хранитcя список (обозначение физ.величины, ее наименование, размерность ит.д.)
;;; ДНк найденный код (дано-найти код) строится в процессе решения конкретной задачи



;; конктруктор ДНк задачи по РНк
(defun construct-dnk (rnk)(cond
				     ((null (car rnk)) nil)
				     ((numberp (cadar rnk)) 
				      (cons (car rnk) (construct-dnk (cdr rnk))))
				     ((ph-vol (cadar rnk))
				      (cons (append (list
						     (caar rnk) 
					             (strnum2num (ph-vol (cadar rnk))))
					     (cddar rnk))
					    (construct-dnk(cdr rnk))))
				     (T (construct-dnk (cdr rnk)))))

;; макрос и его обертка, позволяющая получить имя функции для дальнейшего использования
(defmacro fun-name (f) `(function ,f))
(defun fun2name (l)
  (list 'fun-name l))

;; строит список из первых двух символов списка
(defun ccar (str) (if (listp str) (list (car str) (cadr str)) nil))

;; выводит список параметров функции из ДНк в текстовой форме и упорядочено
(defun param-dnk (dnk) (sort (mapcar #'string (mapcar #'car dnk)) 'string>))

;; ищет и выводит функцию из базы, соответствующую ДНк по списку параметров, сравнивая с предыдущей ф-цией
(defun get-fun (dnk funbase) (cond
					((null (car funbase)) NIL)
					((equal (sort (mapcar #'string (cadar funbase)) 'string>)
						(param-dnk dnk))
					 (car funbase))
					(T (get-fun dnk (cdr funbase)))))
					 


;;; верхний уровень. интерфейс работы программы

;; функция вычисление ответа использует заранее построенную ДНк задачи и базу известных функций
(defun solve-fun-task (dnk funbase) (if (get-fun dnk funbase)
						 (apply 
						  (car (get-fun dnk funbase)) 
						  (to-list1 (mapcar #'ccar dnk))) nil))
;; функция вывода ответа
;; формально -- точка входа в программу
;; на вход подается только текст задачи
;; РНк и база известных функций строятся отдельно (вручную или функциями)
(defun solve (task) (let ((*task* task))
			       (progn
			       (format t "Ответ:")
			       (solve-fun-task (construct-dnk *rnk*) *funbase*))))

;; для построение базы известных ф-ций только по их внешнему виду (ф-обертка) 
(defun add-fun (str) (to-funbase str))
;; для построение РНк базы
(defun add-rnk (rnk) (setf *rnk* (cons rnk *rnk*)))
