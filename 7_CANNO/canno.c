/* 1. form the 2-d processes
 * 2. broadcast A in rows
 * 3. calculate aXb
 * 4. move data B in columns
 */
#include "myhead.h"

void mulmm( a, lda, b, ldb,  c, ldc, m, k, n )
int m, k, n, lda ,ldb, ldc;
float *a, *b, *c;
{
  int i, j, l;

  for(i=0; i<m; i++)
    for(j=0; j<n; j++)
      for(l=0; l<k; l++)
        c[i*ldc+j]+=a[i*lda+l]*b[l*ldb+j];

  return;
}

/*aij=i+j*/
void inita( a, lda, m, myrow, mycol )
int lda, m, myrow, mycol;
float *a;
{
  int offi, offj, i, j;

  offi = myrow*m;
  offj = mycol*m;

  for(i=0; i<m; i++)
    for(j=0; j<m; j++)
      a[i*lda+j]=offi+i+offj+j;

  return;
}

/*bij=1-2*(i+j)%2*/
void initb( a, lda, m, myrow, mycol )
int lda, m, myrow, mycol;
float *a;
{
  int offi, offj, i, j;

  offi = myrow*m;
  offj = mycol*m;

  for(i=0; i<m; i++)
    for(j=0; j<m; j++)
      a[i*lda+j]=1.0-2.0*((offi+i+offj+j)%2);

  return;
}

void initc( a, lda, m )
int lda, m;
float *a;
{
  int i, j;

  for(i=0; i<m; i++)
    for(j=0; j<m; j++)
      a[i*lda+j]=0.0;

  return;
}

/*b=a*/
void copyab( a, lda, b, ldb, m )
int lda, ldb, m;
float *a, *b;
{
  int i, j;

  for(i=0; i<m; i++)
    for(j=0; j<m; j++)
      b[i*ldb+j] = a[i*lda+j];

  return;
}

void cannon(m, a, lda, b, ldb, c, ldc, w, ldw, \
            comm, iam, np, p )
int m, lda, ldb, ldc, ldw, iam, np, p;
float *a, *b, *c, *w;
MPI_Comm comm;
{
  int myrow, mycol; 
  MPI_Comm rowcom, colcom;
  MPI_Datatype matw, matb;
  int i, root, front, next;
  MPI_Status st;
  
  if(np < p*p) return;

  mesh(iam, np, comm, p, p, &myrow, &mycol,\
       &rowcom, &colcom);
  if(iam >= p*p) return;

  inita( a, lda, m, myrow, mycol );
  initb( b, ldb, m, myrow, mycol );
  initc( c, ldc, m );

  MPI_Type_vector( m, m, ldw, MPI_FLOAT, &matw);
  MPI_Type_vector( m, m, ldb, MPI_FLOAT, &matb);
  MPI_Type_commit( &matw );
  MPI_Type_commit( &matb );

  front = ( p+myrow-1)%p;
  next = (myrow+1)%p;

  for (i=0; i<p; i++) {
    /*broadcast a in row direction*/
    root = (myrow+i)%p; 
    if( mycol == root )
      copyab(a, lda, w, ldw, m);
    MPI_Bcast(w, 1, matw, root, rowcom);
    mulmm( w, ldw, b, ldb,  c, ldc, m, m, m );
    if(i == p-1) continue;
    MPI_Sendrecv(b, 1, matb, front, 3, \
                 w, 1, matw, next, 3, colcom, &st); 
    copyab(w, ldw, b, ldb, m);
  }
  MPI_Type_free(&matw);
  MPI_Type_free(&matb);

  MPI_Comm_free(&rowcom);
  MPI_Comm_free(&colcom);
  return;
}

