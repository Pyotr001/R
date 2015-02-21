add2 <- function(x, y) {
  x + y
}

above10 <- function(x){
  x[x>5]
}

above11 <- function(x) {
  use <- x > 11
  x[use]
}

above <- function(x, n) {
  use <- x > n
  x[use]
}

above_n<- function(x, n = 5) {  # значение по умалчанию указано = 5ы
  use <- x > n
  x[use]
}



mean_col <- function(x){        # один аргумент
  for(i in seq_len(ncol(x))) {  # создаём вектор(последовательность) длинной с количество колонок
    print(mean(x[ ,i]))         # печатем каждое среднее из колонки
  }
}                               # функция вычисления средних из колонок матрицы или фрейма


mean_col2 <- function(x, removeNA = TRUE ){
  nc <- ncol(x)         # количество колонок
  means <- numeric(nc)  # создаем вектор длинной с количество столбцов и всеми нулями
  for(i in 1:nc){       # последовательность от 1 до длина вектора
    means[i] <- mean(x[, i], na.rm = removeNA)  # в каждое занчение вектора записываем среднее
                                                # в атрибут na.rm передаём наш атрибут removeNA из значения по умолчанию
                                               #  есть значение по умолчанию TRUE
  }
  means                 # возвращаем вектор со средними
}


f <- function(x) {
  g <- function(y) {
    y + z
  }
  z <- 4
  x + g(x)
}