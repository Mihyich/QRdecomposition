import math

def create_identity_matrix(size):
    """Единичная матрица"""
    return [[1.0 if i == j else 0.0 for j in range(size)] for i in range(size)]

def copy_matrix(matrix):
    """Копия матрицы"""
    return [row[:] for row in matrix]

def compute_givens_rotation(a, b):
    """
    Параметры вращения Гивенса (c, s, r) для элементов a и b, где r = sqrt(a^2 + b^2)
    """
    r = math.hypot(a, b)
    c = a / r
    s = -b / r
    return c, s, r

def apply_givens_rotation(matrix, c, s, i, j, start_col=0):
    """
    Вращение Гивенса к матрице для строк i и j, начиная со столбца start_col
    """
    n_cols = len(matrix[0])
    for k in range(start_col, n_cols):
        temp_i = c * matrix[i][k] - s * matrix[j][k]
        temp_j = s * matrix[i][k] + c * matrix[j][k]
        matrix[i][k] = temp_i
        matrix[j][k] = temp_j

def givens_qr_decomposition(A):
    """
    QR-разложение матрицы A методом вращений Гивенса
    
    Параметры:
        A (list of lists): входная матрица (m x n)
        
    Возврат:
        Q (list of lists): ортогональная матрица (m x m)
        R (list of lists): верхняя треугольная матрица (m x n)
    """
    m = len(A)
    n = len(A[0])
    
    Q = create_identity_matrix(m)
    R = copy_matrix(A)
    
    for j in range(n):
        for i in range(m-1, j, -1):
            if R[i][j] != 0:
                c, s, _ = compute_givens_rotation(R[j][j], R[i][j])
                
                # Вращение к R
                apply_givens_rotation(R, c, s, j, i, j)
                
                # Вращение к Q (транспонированное)
                apply_givens_rotation(Q, c, s, j, i, 0)
    
    Q = [[Q[i][j] for i in range(m)] for j in range(m)]
    
    return Q, R

def matrix_multiply(A, B):
    """Умножение матриц A (m x n) и B (n x p)"""
    m = len(A)
    n = len(A[0])
    p = len(B[0])
    
    result = [[0.0 for _ in range(p)] for _ in range(m)]
    
    for i in range(m):
        for j in range(p):
            for k in range(n):
                result[i][j] += A[i][k] * B[k][j]
    
    return result

def print_matrix(matrix, label=""):
    if label:
        print(label)
    for row in matrix:
        print(' '.join([f"{elem:10.6f}" for elem in row]))
    print()

def main():
    # Пример матрицы
    A = [
        [1, 2],
        [3, 4]
    ]
    
    print_matrix(A, "Исходная матрица A:")
    
    Q, R = givens_qr_decomposition(A)
    
    print_matrix(Q, "Матрица Q:")
    print_matrix(R, "Матрица R:")
    
    # Проверка: A = QR
    QR = matrix_multiply(Q, R)
    print_matrix(QR, "Проверка QR=A:")

if __name__ == "__main__":
    main()