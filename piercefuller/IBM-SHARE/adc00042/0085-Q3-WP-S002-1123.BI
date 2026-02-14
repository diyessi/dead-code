376300013762 000000000000 000040140000 000000004014 000000000000 000020023763 000000000000 000000000000
000000000000 000000000000 000000000000 000017600005 577300035770 200400002004 200400000000 200400000000
000000000000 000040005773 000000000000 000000000000 000000000000 000000000000 000000000000 000017740003
101020041200 100002000000 100010000000 000000000000 000000000000 000000000000 000000000000 000000000000
000000000000 000000000000 000000000000 000000000000 000000000000 000000000000 000000000000 000000000000
000000000000 000000000000 000000000000 000000000000 000000000000 000000000000 000000000000 000000000000
C2 ML HPRS 05-03-61 1124         FO 0149                   7090 FORTRAN         
      SUBROUTINE MULLER(COE,N1,ROOTR,ROOTI)                             HPRS0001
      DIMENSION COE(16),ROOTR(15),ROOTI(15)                             HPRS0002
      N2=N1+1                                                           HPRS0003
      N4=0                                                              HPRS0004
      I=N1+1                                                            HPRS0005
   19 IF(COE(I))9,7,9                                                   HPRS0006
    7 N4=N4+1                                                           HPRS0007
      ROOTR(N4)=0.                                                      HPRS0008
      ROOTI(N4)=0.                                                      HPRS0009
      I=I-1                                                             HPRS0010
      IF(N4-N1)19,37,19                                                 HPRS0011
    9 CONTINUE                                                          HPRS0012
   10 AXR=0.8                                                           HPRS0013
      AXI=0.                                                            HPRS0014
      L=1                                                               HPRS0015
      N3=1                                                              HPRS0016
      ALP1R=AXR                                                         HPRS0017
      ALP1I=AXI                                                         HPRS0018
      M=1                                                               HPRS0019
      GOTO99                                                            HPRS0020
   11 BET1R=TEMR                                                        HPRS0021
      BET1I=TEMI                                                        HPRS0022
      AXR=0.85                                                          HPRS0023
      ALP2R=AXR                                                         HPRS0024
      ALP2I=AXI                                                         HPRS0025
      M=2                                                               HPRS0026
      GOTO99                                                            HPRS0027
   12 BET2R=TEMR                                                        HPRS0028
      BET2I=TEMI                                                        HPRS0029
      AXR=0.9                                                           HPRS0030
      ALP3R=AXR                                                         HPRS0031
      ALP3I=AXI                                                         HPRS0032
      M=3                                                               HPRS0033
      GOTO99                                                            HPRS0034
   13 BET3R=TEMR                                                        HPRS0035
      BET3I=TEMI                                                        HPRS0036
   14 TE1=ALP1R-ALP3R                                                   HPRS0037
      TE2=ALP1I-ALP3I                                                   HPRS0038
      TE5=ALP3R-ALP2R                                                   HPRS0039
      TE6=ALP3I-ALP2I                                                   HPRS0040
      TEM=TE5*TE5+TE6*TE6                                               HPRS0041
      TE3=(TE1*TE5+TE2*TE6)/TEM                                         HPRS0042
      TE4=(TE2*TE5-TE1*TE6)/TEM                                         HPRS0043
      TE7=TE3+1.                                                        HPRS0044
      TE9=TE3*TE3-TE4*TE4                                               HPRS0045
      TE10=2.*TE3*TE4                                                   HPRS0046
      DE15=TE7*BET3R-TE4*BET3I                                          HPRS0047
      DE16=TE7*BET3I+TE4*BET3R                                          HPRS0048
      TE11=TE3*BET2R-TE4*BET2I+BET1R-DE15                               HPRS0049
      TE12=TE3*BET2I+TE4*BET2R+BET1I-DE16                               HPRS0050
      TE7=TE9-1.                                                        HPRS0051
      TE1=TE9*BET2R-TE10*BET2I                                          HPRS0052
      TE2=TE9*BET2I+TE10*BET2R                                          HPRS0053
      TE13=TE1-BET1R-TE7*BET3R+TE10*BET3I                               HPRS0054
      TE14=TE2-BET1I-TE7*BET3I-TE10*BET3R                               HPRS0055
      TE15=DE15*TE3-DE16*TE4                                            HPRS0056
      TE16=DE15*TE4+DE16*TE3                                            HPRS0057
      TE1=TE13*TE13-TE14*TE14-4.*(TE11*TE15-TE12*TE16)                  HPRS0058
      TE2=2.*TE13*TE14-4.*(TE12*TE15+TE11*TE16)                         HPRS0059
      TEM=SQRTF(TE1*TE1+TE2*TE2)                                        HPRS0060
      IF(TE1)113,113,112                                                HPRS0061
  113 TE4=SQRTF(.5*(TEM-TE1))                                           HPRS0062
      TE3=.5*TE2/TE4                                                    HPRS0063
      GO TO 111                                                         HPRS0064
  112 TE3=SQRTF(.5*(TEM+TE1))                                           HPRS0065
      IF(TE2)110,200,200                                                HPRS0066
  110 TE3=-TE3                                                          HPRS0067
  200 TE4=.5*TE2/TE3                                                    HPRS0068
  111 TE7=TE13+TE3                                                      HPRS0069
      TE8=TE14+TE4                                                      HPRS0070
      TE9=TE13-TE3                                                      HPRS0071
      TE10=TE14-TE4                                                     HPRS0072
      TE1=2.*TE15                                                       HPRS0073
      TE2=2.*TE16                                                       HPRS0074
      IF(TE7*TE7+TE8*TE8-TE9*TE9-TE10*TE10)204,204,205                  HPRS0075
  204 TE7=TE9                                                           HPRS0076
      TE8=TE10                                                          HPRS0077
  205 TEM=TE7*TE7+TE8*TE8                                               HPRS0078
      TE3=(TE1*TE7+TE2*TE8)/TEM                                         HPRS0079
      TE4=(TE2*TE7-TE1*TE8)/TEM                                         HPRS0080
      AXR=ALP3R+TE3*TE5-TE4*TE6                                         HPRS0081
      AXI=ALP3I+TE3*TE6+TE4*TE5                                         HPRS0082
      ALP4R=AXR                                                         HPRS0083
      ALP4I=AXI                                                         HPRS0084
      M=4                                                               HPRS0085
      GO TO 99                                                          HPRS0086
   15 N6=1                                                              HPRS0087
   38 IF(ABSF(HELL)+ABSF(BELL)-1.E-20)18,18,16                          HPRS0088
   16 TE7=ABSF(ALP3R-AXR)+ABSF(ALP3I-AXI)                               HPRS0089
      IF(TE7/(ABSF(AXR)+ABSF(AXI))-1.E-7)18,18,17                       HPRS0090
   17 N3=N3+1                                                           HPRS0091
      ALP1R=ALP2R                                                       HPRS0092
      ALP1I=ALP2I                                                       HPRS0093
      ALP2R=ALP3R                                                       HPRS0094
      ALP2I=ALP3I                                                       HPRS0095
      ALP3R=ALP4R                                                       HPRS0096
      ALP3I=ALP4I                                                       HPRS0097
      BET1R=BET2R                                                       HPRS0098
      BET1I=BET2I                                                       HPRS0099
      BET2R=BET3R                                                       HPRS0100
      BET2I=BET3I                                                       HPRS0101
      BET3R=TEMR                                                        HPRS0102
      BET3I=TEMI                                                        HPRS0103
      IF(N3-100)14,18,18                                                HPRS0104
   18 N4=N4+1                                                           HPRS0105
      ROOTR(N4)=ALP4R                                                   HPRS0106
      ROOTI(N4)=ALP4I                                                   HPRS0107
      N3=0                                                              HPRS0108
   41 IF(N4-N1)30,37,37                                                 HPRS0109
   37 RETURN                                                            HPRS0110
   30 IF(ABSF(ROOTI(N4))-1.E-5)10,10,31                                 HPRS0111
   31 GO TO(32,10),L                                                    HPRS0112
   32 AXR=ALP1R                                                         HPRS0113
      AXI=-ALP1I                                                        HPRS0114
      ALP1I=-ALP1I                                                      HPRS0115
      M=5                                                               HPRS0116
      GO TO 99                                                          HPRS0117
   33 BET1R=TEMR                                                        HPRS0118
      BET1I=TEMI                                                        HPRS0119
      AXR=ALP2R                                                         HPRS0120
      AXI=-ALP2I                                                        HPRS0121
      ALP2I=-ALP2I                                                      HPRS0122
      M=6                                                               HPRS0123
      GO TO 99                                                          HPRS0124
   34 BET2R=TEMR                                                        HPRS0125
      BET2I=TEMI                                                        HPRS0126
      AXR=ALP3R                                                         HPRS0127
      AXI=-ALP3I                                                        HPRS0128
      ALP3I=-ALP3I                                                      HPRS0129
      L=2                                                               HPRS0130
      M=3                                                               HPRS0131
   99 TEMR=COE(1)                                                       HPRS0132
      TEMI=0.0                                                          HPRS0133
      DO100I=1,N1                                                       HPRS0134
      TE1=TEMR*AXR-TEMI*AXI                                             HPRS0135
      TEMI=TEMI*AXR+TEMR*AXI                                            HPRS0136
  100 TEMR=     TE1+COE(I+1)                                            HPRS0137
      HELL=TEMR                                                         HPRS0138
      BELL=TEMI                                                         HPRS0139
   42 IF(N4)102,103,102                                                 HPRS0140
  102 DO101I=1,N4                                                       HPRS0141
      TEM1=AXR-ROOTR(I)                                                 HPRS0142
      TEM2=AXI-ROOTI(I)                                                 HPRS0143
      TE1=TEM1*TEM1+TEM2*TEM2                                           HPRS0144
      TE2=(TEMR*TEM1+TEMI*TEM2)/TE1                                     HPRS0145
      TEMI=(TEMI*TEM1-TEMR*TEM2)/TE1                                    HPRS0146
  101 TEMR=TE2                                                          HPRS0147
  103 GO TO(11,12,13,15,33,34),M                                        HPRS0148
      END                                                               HPRS0149
