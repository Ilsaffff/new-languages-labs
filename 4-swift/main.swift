import Foundation

struct Student {
    let name: String
    let grades: [Int]
    
    var averageGrade: Double {
        return Double(grades.reduce(0, +)) / Double(grades.count)
    }
}

func writeToFile(filename: String, content: String) {
    let fileURL = URL(fileURLWithPath: filename)
    do {
        try content.write(to: fileURL, atomically: true, encoding: .utf8)
        print("Результаты записаны в файл '\(filename)'.")
    } catch {
        print("Ошибка записи в файл: \(error)")
    }
}

func readFromFile(filename: String) -> String? {
    let fileURL = URL(fileURLWithPath: filename)
    do {
        let content = try String(contentsOf: fileURL, encoding: .utf8)
        return content
    } catch {
        print("Ошибка чтения из файла: \(error)")
        return nil
    }
}

func processStudentGrades(inputFilename: String, outputFilename: String) {
    if let fileContent = readFromFile(filename: inputFilename) {
        var result = ""
        var students: [Student] = []
        
        let lines = fileContent.split(separator: "\n")
        for line in lines {
            let components = line.split(separator: " ")
            guard components.count > 1 else { continue }
            
            let name = String(components[0])
            let grades = components.dropFirst().compactMap { Int($0) }
            
            let student = Student(name: name, grades: grades)
            students.append(student)
            
            let average = student.averageGrade
            result += "\(student.name): Средний балл = \(average)\n"
        }
        
        // Определяем самого успевающего и неуспевающего студента
        if let bestStudent = students.max(by: { $0.averageGrade < $1.averageGrade }),
           let worstStudent = students.min(by: { $0.averageGrade < $1.averageGrade }) {
            result += "\nСамый успевающий студент: \(bestStudent.name) со средним баллом \(bestStudent.averageGrade)"
            result += "\nСамый неуспевающий студент: \(worstStudent.name) со средним баллом \(worstStudent.averageGrade)"
        }
        
        // Записываем результаты в файл
        writeToFile(filename: outputFilename, content: result)
    }
}

// Пример использования
let inputFilename = "students.txt"
let outputFilename = "results.txt"

// Пример данных, которые можно использовать для теста
let sampleData = """
Иванов 85 90 78
Петров 92 88 79
Сидоров 74 80 85
"""

writeToFile(filename: inputFilename, content: sampleData)

processStudentGrades(inputFilename: inputFilename, outputFilename: outputFilename)