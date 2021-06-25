import numpy as np
import scipy.linalg
from numpy.linalg import cond, norm
from scipy.linalg import toeplitz
from scipy.linalg import solve_triangular
import time
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
sns.set_style('darkgrid')

PI = np.pi
CRED = '\033[91m'
CGREEN = '\033[32m'
CEND = '\033[0m'

def red_print(msg):
    print('Partial Time ' + CRED + str(msg) + CEND)
    
def green_print(msg):
    print('Full Time ' + CGREEN + str(msg) + CEND)

class LU_eliminator(object):
    
    def __init__(self, mode):
        assert mode in ['partial','full']
        self.mode = mode
        return

    def perform_LU_analysis_partial(self, A):
        # Make sure the matrix is square
        assert A.shape[0] == A.shape[1]
        
        # Let m be the number of rows/columns.
        m = A.shape[0]
        
        # Initialize the LU matrix as a copy of A
        # In order to perform in-place substitutions

        LU = np.matrix(np.copy(A))
        
        # Initialize the Permutation Matrix P
        P = np.arange(m)
        
        # Start Timer
        start = time.time()
        # For every row i in the matrix.
        for i in range(0, m-1):
            
            # Find the pivot point (absolute maximum) location on current lower-right matrix.
            p = np.argmax(np.abs(LU[i:,i].ravel()))
             
            # Swap positions in the Permutation Matrix
            P[[i,p+i]] = P[[p+i,i]]
            
            # Swap Rows in the LU matrix.
            # We can use the One-Liner Python Idiom a,b = b,a here.
            LU[[i,p+i],:] = LU[[p+i,i],:]
            
            # Get the Weight Vector that each subsequent row must be multiplied with
            # for the elimination step.
            w = LU[i+1:m,i] / LU[i,i]

            # Perform Elimination on the U part of the LU
            LU[i+1:m,i:m] = LU[i+1:m,i:m] - w*LU[i,i:m]
            
            # Update with the weight the L part of the LU
            LU[i+1:m,i] = w
        
        end = time.time()
        elapsed = (end*1000 -start*1000)
        L = np.tril(LU,-1) + np.eye(m)
        U = np.triu(LU)
        P_ = np.eye(m)
        P = P_[P,:]
        return L,U,P,elapsed

    def perform_LU_analysis_full(self, A):
        # Make sure the matrix is square
        assert A.shape[0] == A.shape[1]
        
        # Let m be the number of rows/columns.
        m = A.shape[0]
        
        # Initialize the LU matrix as a copy of A
        # In order to perform in-place substitutions

        LU = np.matrix(np.copy(A))
        
        # Initialize the Permutation Matrix P
        P = np.arange(m)
        
        # Initialize the Permutation Matrix Q
        Q = np.arange(m)
        
        start = time.time()
        # For every row i in the matrix.
        for i in range(0, m-1):
            
            # Find the pivot point pair id (absolute maximum row / absolute maximum column) location on current lower-right matrix.
            p = np.argmax(np.abs(LU[i:,i:]).ravel())

            # Convert it to a 2D pair given the current lower-right shape
            p_r, p_c = np.unravel_index(p, LU[i:,i:].shape)

            # Swap positions in the Row Permutation Matrix
            P[[i,p_r+i]] = P[[p_r+i,i]]
            
            # Swap positions in the Column Permutation Matrix
            Q[[i,p_c+i]] = Q[[p_c+i,i]]
            
            # Swap Rows in the LU matrix.
            # We can use the One-Liner Python Idiom a,b = b,a here.
            LU[[i,p_r+i],:] = LU[[p_r+i,i],:]
            
            # Swap Columns in the LU matrix.
            # We can use the One-Liner Python Idiom a,b = b,a here.
            LU[:,[i,p_c+i]] = LU[:,[p_c+i,i]]
            
            # Get the Weight Vector that each subsequent row must be multiplied with
            # for the elimination step.
            w = LU[i+1:m,i] / LU[i,i]

            # Perform Elimination on the U part of the LU
            LU[i+1:m,i:m] = LU[i+1:m,i:m] - w*LU[i,i:m]
            
            # Update with the weight the L part of the LU
            LU[i+1:m,i] = w

        end = time.time()
        elapsed = (end*1000 - start*1000)
        L = np.tril(LU,-1) + np.eye(m)
        U = np.triu(LU)
        P_ = np.eye(m)
        P = P_[P,:]
        Q_ = np.eye(m)
        Q = Q_[:,Q]
        return L,U,P,Q,elapsed

    def linear_solve_partial(self, A, b):
        L, U, P, elapsed = self.perform_LU_analysis_partial(A=A)
        Y=scipy.linalg.solve(L,P@b)
        X=scipy.linalg.solve(U,Y)
        return X , elapsed
    
    def linear_solve_full(self, A, b):
        L, U, P, Q,  elapsed = self.perform_LU_analysis_full(A=A)
        Z=scipy.linalg.solve(L,P@b)
        Y=scipy.linalg.solve(U,Z)
        X=scipy.linalg.solve(Q.T,Y)
        return X , elapsed
    
    def linear_solve(self, A, b):
        if self.mode == 'partial':
            X, elapsed = self.linear_solve_partial(A=A,b=b)
        elif self.mode == 'full':
            X, elapsed = self.linear_solve_full(A=A,b=b)
        return X, elapsed
        
def run_exersize_2():
    condition_numbers = []
    
    cpu_times_partial = []
    cpu_times_full = []
    
    error_partial = []
    error_full = []
    
    res_partial = []
    res_full = []
    
    def k_diag_value_calc(k):
        return ((4*(-1)**k) * ((PI**2) * (k**2)-6)) / k**4
    
    def create_toeplitz_matrix(size):
        diag_value = PI ** 4 / 5.0
        diagonals = np.array([diag_value] + [k_diag_value_calc(f) for f in range(1,size)])
        return toeplitz(diagonals)
    
    
    sizes = [64,128,256,512,1024,2048]
    x_list = [np.random.randn(f,1) for f in sizes]
    A_list = [create_toeplitz_matrix(f) for f in sizes]
    b_list = [np.matmul(x1,x2) for x1,x2 in zip(A_list,x_list)]
    
    for A,b,x in zip(A_list, b_list, x_list):
        print(norm(A,np.inf))
        condition_numbers.append(cond(A, np.inf))
        partial_solver = LU_eliminator(mode='partial')
        full_solver = LU_eliminator(mode='full')
        
        px, ptime = partial_solver.linear_solve(A=A,b=b)
        fx, ftime = full_solver.linear_solve(A=A,b=b)
        
        perror = norm(px-x, np.inf)
        ferror = norm(fx-x, np.inf)
        
        pres = norm(b-A@px, np.inf)
        fres = norm(b-A@fx, np.inf)
        
        cpu_times_partial.append(ptime)
        cpu_times_full.append(ftime)
        
        error_partial.append(perror)
        error_full.append(ferror)
        
        res_partial.append(pres)
        res_full.append(fres)


    df = pd.DataFrame(data={'size':sizes, 'condition_number':condition_numbers, 'cpu_times_partial':cpu_times_partial, 'cpu_times_full':cpu_times_full, 'error_partial':error_partial, 'error_full':error_full, 'res_partial':res_partial, 'res_full':res_full})
    return df

def run_exersize_4():
    
    condition_numbers = []
    
    cpu_times_partial = []
    cpu_times_full = []
    
    error_partial = []
    error_full = []
    
    res_partial = []
    res_full = []

    def create_custom_matrix(size):
        A = np.ones((size,size))*(-1)
        A *= np.tri(*A.shape,-1)
        A += np.eye(size)
        A[:, size-1] = np.ones(size)
        return A
    
    sizes = [64,128,256,512,1024]
    x_list = [np.random.randn(f,1) for f in sizes]
    A_list = [create_custom_matrix(f) for f in sizes]
    b_list = [np.matmul(x1,x2) for x1,x2 in zip(A_list,x_list)]
    
    for A,b,x in zip(A_list, b_list, x_list):

        condition_numbers.append(cond(A, np.inf))
        partial_solver = LU_eliminator(mode='partial')
        full_solver = LU_eliminator(mode='full')
        
        px, ptime = partial_solver.linear_solve(A=A,b=b)
        fx, ftime = full_solver.linear_solve(A=A,b=b)

        perror = norm(px-x, np.inf)
        ferror = norm(fx-x, np.inf)
        
        
        pres = norm(b-A@px, np.inf)
        fres = norm(b-A@fx, np.inf)
            
        error_partial.append(perror)
        error_full.append(ferror)
        
        cpu_times_partial.append(ptime)
        cpu_times_full.append(ftime)
        
        
        res_partial.append(pres)
        res_full.append(fres)

    df = pd.DataFrame(data={'size':sizes, 'condition_number':condition_numbers, 'cpu_times_partial':cpu_times_partial, 'cpu_times_full':cpu_times_full, 'error_partial':error_partial, 'error_full':error_full, 'res_partial':res_partial, 'res_full':res_full})
    return df
 
def run_exersize_5():

    condition_numbers = []
    cpu_times_partial = []
    error_partial = []
    res_partial = []
    
    def k_diag_value_calc(k):
        return ((4*(-1)**k) * ((PI**2) * (k**2)-6)) / k**4
    
    def create_toeplitz_matrix(size):
        diag_value = PI ** 4 / 5.0
        diagonals = np.array([diag_value] + [k_diag_value_calc(f) for f in range(1,size)])
        return toeplitz(diagonals)
    
    def l2_normalize(v):
        v = v.astype(float)
        norm = np.linalg.norm(v)
        if norm == 0: 
            return v
        return v / norm
    
    sizes = [64,128,256,512,1024]
    x_list = [np.random.randn(f,1) for f in sizes]
    u_list = [l2_normalize(np.random.randn(f,1)) for f in sizes]
    v_list = [l2_normalize(np.random.randn(f,1)) for f in sizes]
    A_list = [create_toeplitz_matrix(f) for f in sizes]
    b_list = [np.matmul((A + np.outer(u,v)), x) for A,u,v,x in zip(A_list,u_list,v_list,x_list)]
    


    for A,b,x,u,v in zip(A_list, b_list, x_list, u_list, v_list):
        condition_numbers.append(cond(A, np.inf))
        partial_solver = LU_eliminator(mode='partial')
        L, U, P, _ = partial_solver.perform_LU_analysis_partial(A=A)

        # Start time here because Sherman-Morisson Assumes
        # Prior Knowledge of LU factorization

        # Start Timer
        start = time.time()

        # Assert Norm = 1
        print(norm(u,2))
        print(norm(v,2))



        # Partial Problem 1 Solve Az = u for z, so z = A^-1 * u
        # Forward
        pp1 = solve_triangular(L,P@u,lower=True)
        # Backward
        z = solve_triangular(U,pp1,lower=False)



        # Partial Problem 2 Solve Ay = b for y, so y = A^-1 * b
        # Forward
        pp2 = solve_triangular(L,P@b,lower=True)
        # Backward
        y = solve_triangular(U,pp2,lower=False)

        # Plug-In and solve
        vz = v.T@z
        vz = vz[0]
        vy = v.T@y
        vy = vy[0]
        calc = vy/(1-vz)
        z = calc * z
        px = y + z
        end = time.time()
        elapsed = (end -start) * 1000
        perror = norm(px-x, np.inf)
        res_part = norm(b-A@px, np.inf)
        error_partial.append(perror)
        res_partial.append(res_part)
        cpu_times_partial.append(elapsed)


    df = pd.DataFrame(data={'size':sizes, 'condition_number':condition_numbers, 'error_partial':error_partial, 'cpu_times_partial':cpu_times_partial, 'res_partial':res_partial})
    return df


# Exersize 2

# df = run_exersize_2()
# df.to_csv('Exersize2.csv', index=False)
# cn = df['condition_number'].values
# cpu_part = df['cpu_times_partial'].values
# cpu_full = df['cpu_times_full'].values
# error_partial = df['error_partial'].values
# error_full = df['error_full'].values
# res_part = df['res_partial'].values
# res_full = df['res_full'].values

# plt.figure(figsize=(8,8))
# plt.title('Excxecution Time vs Condition Number')
# plt.plot(cn,cpu_part,'ro--',label='Partial Pivoting Excecution Time(ms)')
# plt.plot(cn,cpu_full,'bo--',label='Full Pivoting Excecution Time(ms)')
# plt.legend(loc=2)
# plt.savefig('Cpu_Time_vs_CN_2.png')
# plt.show()
# plt.close()


# plt.figure(figsize=(8,8))
# plt.title('Error vs Condition Number')
# plt.plot(cn,error_partial,'ro--',label='Partial Pivoting Error')
# plt.plot(cn,error_full,'bo--',label='Full Pivoting Error')
# plt.legend(loc=2)
# plt.savefig('Error_vs_CN_2.png')
# plt.show()
# plt.close()


# plt.figure(figsize=(8,8))
# plt.title('Residual vs Condition Number')
# plt.plot(cn,res_part,'ro--',label='Partial Pivoting Residual')
# plt.plot(cn,res_full,'bo--',label='Full Pivoting Residual')
# plt.legend(loc=2)
# plt.savefig('Residual_vs_CN_2.png')
# plt.show()
# plt.close()


# Exersize 4

# df = run_exersize_4()
# df.to_csv('Exersize4.csv', index=False)
# cn = df['condition_number'].values
# cpu_part = df['cpu_times_partial'].values
# cpu_full = df['cpu_times_full'].values
# error_partial = df['error_partial'].values
# error_full = df['error_full'].values
# res_part = df['res_partial'].values
# res_full = df['res_full'].values

# plt.figure(figsize=(8,8))
# plt.title('Excxecution Time vs Condition Number')
# plt.plot(cn,cpu_part,'ro--',label='Partial Pivoting Excecution Time(ms)')
# plt.plot(cn,cpu_full,'bo--',label='Full Pivoting Excecution Time(ms)')
# plt.legend(loc=2)
# plt.savefig('Cpu_Time_vs_CN_4.png')
# plt.show()
# plt.close()


# plt.figure(figsize=(8,8))
# plt.title('Error vs Condition Number')
# plt.plot(cn,error_partial,'ro--',label='Partial Pivoting Error')
# plt.plot(cn,error_full,'bo--',label='Full Pivoting Error')
# plt.legend(loc=2)
# plt.savefig('Error_vs_CN_4.png')
# plt.show()
# plt.close()


# plt.figure(figsize=(8,8))
# plt.title('Residual vs Condition Number')
# plt.plot(cn,res_part,'ro--',label='Partial Pivoting Residual')
# plt.plot(cn,res_full,'bo--',label='Full Pivoting Residual')
# plt.legend(loc=2)
# plt.savefig('Residual_vs_CN_4.png')
# plt.show()
# plt.close()



# Exersize 5

df = run_exersize_5()
df.to_csv('Exersize5.csv', index=False)
cn = df['condition_number'].values
cpu_part = df['cpu_times_partial'].values
error_partial = df['error_partial'].values
res_part = df['res_partial'].values

plt.figure(figsize=(8,8))
plt.title('Excxecution Time vs Condition Number')
plt.plot(cn,cpu_part,'ro--',label='Partial Pivoting Excecution Time(ms)')
plt.legend(loc=2)
plt.savefig('Cpu_Time_vs_CN_5.png')
plt.show()
plt.close()


plt.figure(figsize=(8,8))
plt.title('Error vs Condition Number')
plt.plot(cn,error_partial,'ro--',label='Partial Pivoting Error')
plt.legend(loc=2)
plt.savefig('Error_vs_CN_5.png')
plt.show()
plt.close()


plt.figure(figsize=(8,8))
plt.title('Residual vs Condition Number')
plt.plot(cn,res_part,'ro--',label='Partial Pivoting Residual')
plt.legend(loc=2)
plt.savefig('Residual_vs_CN_5.png')
plt.show()
plt.close()