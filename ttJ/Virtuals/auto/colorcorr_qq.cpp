      clo[1][1] = -1/N+N;
      clo[2][1] = clo[1][1];
      clo[3][1] = 0.0;
      clo[4][1] = clo[2][1];
      clo[1][2] = clo[4][1];
      t2 = N*N;
      clo[2][2] = -N+t2*N;
      clo[3][2] = clo[1][2];
      clo[4][2] = 0.0;
      clo[1][3] = 0.0;
      clo[2][3] = clo[3][2];
      clo[3][3] = clo[2][3];
      clo[4][3] = clo[3][3];
      clo[1][4] = clo[4][3];
      clo[2][4] = 0.0;
      clo[3][4] = clo[1][4];
      clo[4][4] = clo[2][2];
      t4 = 1/t2;
      cttb[1][1] = 1.0-t4/2.0-t2/2.0;
      cttb[2][1] = cttb[1][1];
      cttb[3][1] = 0.0;
      cttb[4][1] = cttb[2][1];
      cttb[1][2] = cttb[4][1];
      cttb[2][2] = 0.0;
      cttb[3][2] = 1.0/2.0-t4/2.0;
      cttb[4][2] = 1.0/2.0-t2/2.0;
      cttb[1][3] = 0.0;
      cttb[2][3] = cttb[3][2];
      cttb[3][3] = cttb[2][3];
      cttb[4][3] = cttb[3][3];
      cttb[1][4] = cttb[1][2];
      cttb[2][4] = cttb[4][2];
      cttb[3][4] = cttb[4][3];
      cttb[4][4] = 0.0;
      cqqb[1][1] = cttb[3][4];
      cqqb[2][1] = cqqb[1][1];
      cqqb[3][1] = 0.0;
      cqqb[4][1] = cqqb[2][1];
      cqqb[1][2] = cqqb[4][1];
      cqqb[2][2] = 0.0;
      cqqb[3][2] = cttb[1][4];
      cqqb[4][2] = cttb[2][4];
      cqqb[1][3] = 0.0;
      cqqb[2][3] = cqqb[3][2];
      cqqb[3][3] = cqqb[2][3];
      cqqb[4][3] = cqqb[3][3];
      cqqb[1][4] = cqqb[1][2];
      cqqb[2][4] = cqqb[4][2];
      cqqb[3][4] = cqqb[4][3];
      cqqb[4][4] = 0.0;
      cqg3[1][1] = cqqb[2][4];
      cqg3[2][1] = cqg3[1][1];
      cqg3[3][1] = 0.0;
      cqg3[4][1] = cqg3[2][1];
      cqg3[1][2] = cqg3[4][1];
      cqg3[2][2] = 0.0;
      cqg3[3][2] = -cqg3[1][2];
      cqg3[4][2] = 0.0;
      cqg3[1][3] = 0.0;
      cqg3[2][3] = cqg3[3][2];
      cqg3[3][3] = 0.0;
      cqg3[4][3] = cqg3[1][2];
      cqg3[1][4] = cqg3[4][3];
      cqg3[2][4] = 0.0;
      cqg3[3][4] = cqg3[1][4];
      t9 = t2*t2;
      cqg3[4][4] = t2/2.0-t9/2.0;
      cqbg3[1][1] = cqg3[3][4];
      cqbg3[2][1] = cqbg3[1][1];
      cqbg3[3][1] = 0.0;
      cqbg3[4][1] = cqbg3[2][1];
      cqbg3[1][2] = cqbg3[4][1];
      cqbg3[2][2] = cqg3[4][4];
      cqbg3[3][2] = cqbg3[1][2];
      cqbg3[4][2] = 0.0;
      cqbg3[1][3] = 0.0;
      cqbg3[2][3] = cqbg3[3][2];
      cqbg3[3][3] = 0.0;
      cqbg3[4][3] = cqg3[2][3];
      cqbg3[1][4] = cqbg3[2][3];
      cqbg3[2][4] = 0.0;
      cqbg3[3][4] = cqbg3[4][3];
      cqbg3[4][4] = 0.0;
      cqt[1][1] = 0.0;
      cqt[2][1] = cqqb[3][4];
      cqt[3][1] = -cqqb[1][4];
      cqt[4][1] = cqqb[1][4];
      cqt[1][2] = cqt[2][1];
      cqt[2][2] = -1.0/2.0+t2-t9/2.0;
      cqt[3][2] = cqt[1][2];
      cqt[4][2] = 0.0;
      cqt[1][3] = cqt[3][1];
      cqt[2][3] = cqt[3][2];
      cqt[3][3] = 0.0;
      cqt[4][3] = cqt[4][1];
      cqt[1][4] = cqt[4][3];
      cqt[2][4] = 0.0;
      cqt[3][4] = cqt[1][4];
      cqt[4][4] = cqbg3[3][4];
      cqbt[1][1] = 0.0;
      cqbt[2][1] = cqt[1][3];
      cqbt[3][1] = cqt[3][4];
      cqbt[4][1] = -cqt[2][3];
      cqbt[1][2] = cqbt[2][1];
      cqbt[2][2] = 0.0;
      cqbt[3][2] = cqbt[4][1];
      cqbt[4][2] = cqt[4][4];
      cqbt[1][3] = cqbt[3][1];
      cqbt[2][3] = cqbt[3][2];
      cqbt[3][3] = 0.0;
      cqbt[4][3] = cqbt[1][2];
      cqbt[1][4] = cqbt[2][3];
      cqbt[2][4] = cqbt[4][2];
      cqbt[3][4] = cqbt[4][3];
      cqbt[4][4] = 0.0;
      cg3t[1][1] = 0.0;
      cg3t[2][1] = cqbt[2][4];
      cg3t[3][1] = 0.0;
      cg3t[4][1] = cqbg3[1][4];
      cg3t[1][2] = cg3t[2][1];
      cg3t[2][2] = 0.0;
      cg3t[3][2] = cg3t[4][1];
      cg3t[4][2] = 0.0;
      cg3t[1][3] = 0.0;
      cg3t[2][3] = cg3t[3][2];
      cg3t[3][3] = cg3t[2][3];
      cg3t[4][3] = cg3t[3][3];
      cg3t[1][4] = cg3t[4][3];
      cg3t[2][4] = 0.0;
      cg3t[3][4] = cg3t[1][4];
      cg3t[4][4] = cqbg3[2][2];
      cqtb[1][1] = 0.0;
      cqtb[2][1] = cqbt[1][4];
      cqtb[3][1] = cqbt[1][3];
      cqtb[4][1] = cqbt[3][4];
      cqtb[1][2] = cqtb[2][1];
      cqtb[2][2] = 0.0;
      cqtb[3][2] = cqtb[4][1];
      cqtb[4][2] = cg3t[1][2];
      cqtb[1][3] = cqtb[3][1];
      cqtb[2][3] = cqtb[3][2];
      cqtb[3][3] = 0.0;
      cqtb[4][3] = cqtb[1][2];
      cqtb[1][4] = cqtb[2][3];
      cqtb[2][4] = cqtb[4][2];
      cqtb[3][4] = cqtb[4][3];
      cqtb[4][4] = 0.0;
      cqbtb[1][1] = 0.0;
      cqbtb[2][1] = cqtb[1][3];
      cqbtb[3][1] = cqtb[1][4];
      cqbtb[4][1] = cqt[2][3];
      cqbtb[1][2] = cqbtb[2][1];
      cqbtb[2][2] = cqtb[2][4];
      cqbtb[3][2] = cqbtb[1][2];
      cqbtb[4][2] = 0.0;
      cqbtb[1][3] = cqbtb[3][1];
      cqbtb[2][3] = cqbtb[3][2];
      cqbtb[3][3] = 0.0;
      cqbtb[4][3] = cqbtb[4][1];
      cqbtb[1][4] = cqbtb[4][3];
      cqbtb[2][4] = 0.0;
      cqbtb[3][4] = cqbtb[1][4];
      cqbtb[4][4] = cqt[2][2];
      cg3tb[1][1] = 0.0;
      cg3tb[2][1] = cg3t[3][4];
      cg3tb[3][1] = 0.0;
      cg3tb[4][1] = cqbtb[2][2];
      cg3tb[1][2] = cg3tb[2][1];
      cg3tb[2][2] = cg3t[4][4];
      cg3tb[3][2] = cg3tb[1][2];
      cg3tb[4][2] = 0.0;
      cg3tb[1][3] = 0.0;
      cg3tb[2][3] = cg3tb[3][2];
      cg3tb[3][3] = cg3tb[2][3];
      cg3tb[4][3] = cg3tb[3][3];
      cg3tb[1][4] = cg3tb[4][1];
      cg3tb[2][4] = 0.0;
      cg3tb[3][4] = cg3tb[4][3];
      cg3tb[4][4] = 0.0;
