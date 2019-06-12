module m_mpi_hang_lie
        use m_mpi_my
        implicit none
        REAL,allocatable :: Bnode(:,:),Anode(:,:),Cnode(:,:)



        node_n=getn(node,m)
        allocate(Anode(node_m,k),Cnode(node_m,n))

        Do i=0,np-1  !node0 calculate B from node0,1,2,...,np-1
        from_node=mod(node+i,np)+1
        node_n=getn(from_node,n)
        allocate(Bnode(k,node_n))

        Ctmp=matmul(Anode,Bnode)

         

        








End module m_mpi_hang_lie 
