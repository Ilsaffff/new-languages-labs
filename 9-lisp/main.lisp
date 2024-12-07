(defun count-words (text)
  "Функция для подсчета слов в строке."
  (let ((words 0)
        (in-word nil))
    (loop for char across text do
         (if (alpha-char-p char)
             (progn
               (unless in-word
                 (incf words))
               (setq in-word t))
             (setq in-word nil)))
    words))

(defun count-letters (text)
  "Функция для подсчета количества букв в строке."
  (let ((letters (make-hash-table :test 'equal)))
    (loop for char across text do
         (when (alpha-char-p char)
           (incf (gethash char letters 0))))
    letters))

(defun most-frequent-letter (text)
  "Функция для нахождения наиболее частой буквы."
  (let ((letter-counts (count-letters text)))
    (let ((max-char (car (hash-table-keys letter-counts))) ; начнем с первого ключа
          (max-count (gethash (car (hash-table-keys letter-counts)) letter-counts)))
      (dolist (char (hash-table-keys letter-counts) max-char)
        (let ((count (gethash char letter-counts)))
          (when (> count max-count)
            (setq max-count count)
            (setq max-char char)))) 
      max-char)))

(defun hash-table-keys (table)
  "Функция для получения всех ключей хеш-таблицы."
  (loop for key being the hash-keys of table collect key))

(defun process-file (filename)
  "Функция для чтения файла, подсчета слов и поиска наиболее частой буквы."
  (with-open-file (stream filename)
    (let ((text (make-string (file-length stream) :initial-element #\Space)))
      (read-sequence text stream)
      (format t "Количество слов: ~A~%" (count-words text))
      (format t "Наиболее частая буква: ~A~%" (most-frequent-letter text)))))

(process-file "input.txt")