;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname behrakis-nobrega-hw7) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #t)))
;; Kristen Behrakis (kbehrakis) and Emilia Nobrega (emnobrega)

;; QUESTION 1
(define-struct course (department coursenum faculty enrollment all-students))
;; a Course is a (make-course String Number String Natural ListOfStudent)
;; interp: a course where
;;    department is the department offering the course
;;    coursenum is the number of the course
;;    faculty is the faculty member teaching the course
;;    enrollment is the maximum enrollment for the course
;;    all-students is a list of the students currently enrolled in the classs

;; a ListOfStudent is one of
;;    empty
;;    (cons Student ListOfStudent)

(define-struct student (name id courses))
;; a Student is a (make-student String Number ListOfCourse)
;; interp: a student where
;;    name is the name of the student
;;    id is the student's id number
;;    courses is a list of courses the student is enrolled in 

;; a ListOfCourse is one of
;;    empty
;;    (cons Course ListOfCourse)


;; QUESTION 2
;; Courses: ListOfCourse
;; stores a list of courses 
(define Courses empty)

;; Students: ListOfStudent
;; stores a list of students
(define Students empty)


;; QUESTION 3
;; add-student: String Number -> void
;; consumes a student's name and id number and adds the student
;; to Students if the student with the given id is not already
;; in the list and produces an error otherwise
;; EFFECT: changes the content of Students 
(define (add-student stud-name stud-id)
  (if (member? stud-id (map student-id Students))
      (error "student already exists")
      (set! Students (cons (make-student stud-name stud-id empty) Students))))


;; QUESTION 4
;; add-course: String Number String Natural -> void
;; consumes a department, a course number, the name of a faculty member,
;; and the maximum enrollment and adds the course to Courses if the course
;; with the given department and id is not already  in the list and produces an error otherwise
;; EFFECT: changes the content of Courses
(define (add-course a-dept num-course faculty-member max-enroll)
  (if (and (member? a-dept (map course-department Courses))
           (member? num-course (map course-coursenum Courses)))
      (error "course already exists")
      (set! Courses (cons (make-course a-dept num-course faculty-member max-enroll empty) Courses))))

;;QUESTION 5
;;add-to-course: Number String Number -> void
;;consumes a student's ID, a department, and a course number and produces void if
;;the course and student exist and the course has enough room left
;;EFFECT: changes the content of the given course's ListOfStudent and the content
;;         of the given student's ListOfCourse
(define (add-to-course stud-id a-dept num-course)
  (cond [(not (member? stud-id (map student-id Students))) (error "no such student")]
        [(not (and (member? a-dept (map course-department Courses))
                   (member? num-course (map course-coursenum Courses)))) (error "no such course")]
        [(> (course-enrollment (find-course a-dept num-course Courses))
            (length (course-all-students (find-course a-dept num-course Courses))))
         (begin
           (set-course-all-students! (find-course a-dept num-course Courses)
                                     (cons (find-student stud-id Students)
                                           (course-all-students
                                            (find-course a-dept num-course Courses))))
           (set-student-courses! (find-student stud-id Students)
                                 (cons (find-course a-dept num-course Courses)
                                       (student-courses (find-student stud-id Students)))))]
        [else (error "course full")]))

;;find-student: Number ListOfStudent -> Student
;;consumes a student ID number and a list of students that the student with the id number is in and
;;produces the student with the given ID number
(define (find-student stud-id stu-list)
  (if (= stud-id (student-id (first stu-list)))
      (first stu-list)
      (find-student stud-id (rest stu-list))))

;;tests
(define test-stu-list (list (make-student "Bob" 123 empty)
                            (make-student "Stella" 456 empty)
                            (make-student "Chris" 789 empty)))

(check-expect (find-student 123 test-stu-list) (make-student "Bob" 123 empty))
(check-expect (find-student 456 test-stu-list) (make-student "Stella" 456 empty))
(check-expect (find-student 789 test-stu-list) (make-student "Chris" 789 empty))

;;find-course: String Number ListOfCourse -> Course
;;consumes a department, a course ID number and a list of courses that the given course is in and
;;produces the course with the given ID number
(define (find-course a-dept num-course crs-list)
  (if (and (= num-course (course-coursenum (first crs-list)))
           (string=? a-dept (course-department (first crs-list))))
      (first crs-list)
      (find-course a-dept num-course (rest crs-list))))

;;tests
(define test-crs-list (list (make-course "CS" 1101 "Hamel" 100 empty)
                            (make-course "BI" 1000 "Rulfs" 20 empty)
                            (make-course "MA" 2201 "Servatius" 50 empty)))
(check-expect (find-course "CS" 1101 test-crs-list) (make-course "CS" 1101 "Hamel" 100 empty))
(check-expect (find-course "BI" 1000 test-crs-list) (make-course "BI" 1000 "Rulfs" 20 empty))
(check-expect (find-course "MA" 2201 test-crs-list) (make-course "MA" 2201 "Servatius" 50 empty))

;;QUESTION 6
;;largest-enrollment: null -> Course
;;produces the course in the current course list
;;with the largest number of students in its list of students
(define (largest-enrollment)
  (local [(define (largest-enrollment aloc acc)
            (cond [(empty? aloc) acc]
                  [(cons? aloc)(if (> (course-size (first aloc)) (course-size acc))
                                   (largest-enrollment (rest aloc) (first aloc))
                                   (largest-enrollment (rest aloc) acc))]))
          (define (course-size a-crs)
            (length (course-all-students a-crs)))
          (define (check-list aloc)
            (cond [(empty? aloc) (error "No courses currently in Courses")]
                  [(cons? aloc) (rest aloc)]))]
    (largest-enrollment (check-list Courses) (first Courses))))