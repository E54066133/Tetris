PROGRAM   com_third

   INTEGER     :: film(10,7)   !薄膜
   LOGICAL     :: escap        !用於逃脫final_loop之方式(2)
   INTEGER     :: ran_x,ran_y  !用於隨機生成選取位置
   INTEGER     :: x,y          !記錄隨機選取之位置
   INTEGER     :: choose       !用於隨機生成確認符合順序
   INTEGER     :: goal         !用於確認是否所有構成分子都有測試過能否填入
   INTEGER     :: done         !用於確認選取之空位是否成功填入
   LOGICAL     :: success      !用於確認是否完整填完一整張薄膜
   INTEGER     :: try_count    !用於計算嘗試了幾個位置
   LOGICAL     :: conti        !用於確認走到死局要重來
   INTEGER     :: t_shape,l_shape,square  !計算各種類分子出現數目
   INTEGER     :: total                   !計算所有分子出現的數目

   OPEN(unit=11,file="film_color.xyz",status="unknown")
   CALL random_seed
   done=0
   goal=0
   conti=.TRUE.

   outer:DO WHILE(conti)
     !Setup
     DO kk=1,7
        DO k=1,10
           film(k,kk)=0      !尚未填入=0
        ENDDO
     ENDDO
     DO i=1,10
        film(i,1)=10         ! 外圍黑框=10
     ENDDO
     DO i=1,10
        film(i,7)=10         ! 外圍黑框=10
     ENDDO
     DO j=1,7
        film(1,j)=10         ! 外圍黑框=10
     ENDDO
     DO j=1,7
        film(10,j)=10        ! 外圍黑框=10
     ENDDO
     t_shape=0
     l_shape=0
     square=0
     total=0
  
  
     !Steps
     !Steps1 : 隨機選一個空的位置
     final:DO WHILE(.TRUE.)
       DO WHILE(.TRUE.)
          CALL random_number(x_number)
          CALL random_number(y_number)
          ran_x=INT(x_number*100)
          ran_y=INT(y_number*100)
          IF(ran_x>=1 .AND. ran_y>=1)THEN
             x = MOD(ran_x,10)                    !X座標
             y = MOD(ran_y,7)                     !Y座標
             IF(film(x,y)<=0)EXIT                 !確保選到空位
          ENDIF
       ENDDO
       try_count=try_count+1
  
       !Step2 : 確認是否可以填入構成分子
       done=0
       goal=0
       DO WHILE(.TRUE.)
          CALL random_number(z)
          choose=INT(z*100)
          IF(choose>=1)EXIT
       ENDDO
       choose=MOD(choose,7)           !隨機生成確認起始點(依序確認)
  
       fit:DO WHILE(.TRUE.)
          !#1 (上T型)
          IF(film(x,y)<=0 .AND. done==0 .AND. choose==1)THEN
             IF(film(x-1,y)<=0 .AND. film(x,y+1)<=0 .AND. film(x+1,y)<=0)THEN
                film(x,y)=1
                film(x-1,y)=1
                film(x,y+1)=1
                film(x+1,y)=1
                done=1
                t_shape=t_shape+1
             ENDIF
             choose=choose+1
             goal=goal+1
          ENDIF
          IF(goal>=7 .OR. done==1)EXIT fit
          !#2 (右T型)
          IF(film(x,y)<=0 .AND. done==0 .AND. choose==2)THEN
             IF(film(x,y+1)<=0 .AND. film(x,y-1)<=0 .AND. film(x+1,y)<=0)THEN
                film(x,y)=2
                film(x,y+1)=2
                film(x,y-1)=2
                film(x+1,y)=2
                done=1
                t_shape=t_shape+1
             ENDIF
             choose=choose+1
             goal=goal+1
          ENDIF
          IF(goal>=7 .OR. done==1)EXIT fit
          !#3 (下T型)
          IF(film(x,y)<=0 .AND. done==0 .AND. choose==3)THEN
             IF(film(x-1,y)<=0 .AND. film(x+1,y)<=0 .AND. film(x,y-1)<=0)THEN
                film(x,y)=3
                film(x-1,y)=3
                film(x+1,y)=3
                film(x,y-1)=3
                done=1
                t_shape=t_shape+1
             ENDIF
             choose=choose+1
             goal=goal+1
          ENDIF
          IF(goal>=7 .OR. done==1)EXIT fit
          !#4 (左T型)
          IF(film(x,y)<=0 .AND. done==0 .AND. choose==4)THEN
             IF(film(x-1,y)<=0 .AND. film(x,y+1)<=0 .AND. film(x,y-1)<=0)THEN
                film(x,y)=4
                film(x-1,y)=4
                film(x,y+1)=4
                film(x,y-1)=4
                done=1
                t_shape=t_shape+1
             ENDIF
             choose=choose+1
             goal=goal+1
          ENDIF
          IF(goal>=7 .OR. done==1)EXIT fit
          !#5 (右高L型)
          IF(film(x,y)<=0 .AND. done==0 .AND. choose==5)THEN
             IF(film(x-1,y)<=0 .AND. film(x+1,y)<=0 .AND. film(x+1,y+1)<=0)THEN
                film(x,y)=5
                film(x-1,y)=5
                film(x+1,y)=5
                film(x+1,y+1)=5
                done=1
                l_shape=l_shape+1
             ENDIF
             choose=choose+1
             goal=goal+1
          ENDIF
          IF(goal>=7 .OR. done==1)EXIT fit
          !#6 (左低L型)
          IF(film(x,y)<=0 .AND. done==0 .AND. choose==6)THEN
             IF(film(x-1,y)<=0 .AND. film(x+1,y)<=0 .AND. film(x-1,y-1)<=0)THEN
                film(x,y)=6
                film(x-1,y)=6
                film(x+1,y)=6
                film(x-1,y-1)=6
                done=1
                l_shape=l_shape+1
             ENDIF
             choose=choose+1
             goal=goal+1
          ENDIF
          IF(goal>=7 .OR. done==1)EXIT fit
          !#7 (正方形)
          IF(film(x,y)<=0 .AND. done==0 .AND. choose==7)THEN
             !(1)錨點在左下
             IF(film(x,y+1)<=0 .AND. film(x+1,y+1)<=0 .AND. film(x+1,y)<=0 .AND. done==0)THEN
                film(x,y)=7
                film(x,y+1)=7
                film(x+1,y+1)=7
                film(x+1,y)=7
                done=1
                square=square+1
             ENDIF
             !(2)錨點在左上
             IF(film(x,y-1)<=0 .AND. film(x+1,y-1)<=0 .AND. film(x+1,y)<=0 .AND. done==0)THEN
                film(x,y)=7
                film(x,y-1)=7
                film(x+1,y-1)=7
                film(x+1,y)=7
                done=1
                square=square+1
             ENDIF
             !(3)錨點在右上
             IF(film(x-1,y)<=0 .AND. film(x-1,y-1)<=0 .AND. film(x,y-1)<=0 .AND. done==0)THEN
                film(x,y)=7
                film(x-1,y)=7
                film(x-1,y-1)=7
                film(x,y-1)=7
                done=1
                square=square+1
             ENDIF
             !(4)錨點在右下
             IF(film(x-1,y)<=0 .AND. film(x-1,y+1)<=0 .AND. film(x,y+1)<=0 .AND. done==0)THEN
                film(x,y)=7
                film(x-1,y)=7
                film(x-1,y+1)=7
                film(x,y+1)=7
                done=1
                square=square+1
             ENDIF
             choose=choose+1
             goal=goal+1
          ENDIF
          IF(goal<7)THEN
             choose=1
          ENDIF
          IF(goal>=7 .OR. done==1)EXIT fit
       ENDDO fit
       !若擬合失敗則填入-1
       IF(film(x,y)<=0)THEN
          film(x,y)=-1
       ENDIF
  
       !逃脫FINAL_LOOP的條件
       !方式(1)
       IF(film(x,y)<=0 .AND. film(x+1,y)>0 .AND. film(x-1,y)>0 &
          &.AND. film(x,y+1)>0 .AND. film(x,y-1)>0)EXIT final
       !方式(2)
       escap=.FALSE.
       IF(film(x+1,y)==-1 .OR. film(x-1,y)==-1 &
          &.OR. film(x,y+1)==-1 .OR. film(x,y-1)==-1)THEN
          escap=.TRUE.
       ELSE
          escap=.FALSE.
       ENDIF
       IF(film(x,y)==-1 .AND. escap .AND. film(x+1,y)/=0 .AND. &
          &film(x-1,y)/=0 .AND. film(x,y+1)/=0 .AND. film(x,y-1)/=0)EXIT final
       !方式(3)
       count=0
       success=.FALSE.
       IF(MOD(try_count,100)==0)THEN
          DO q=2,6
             DO p=2,9
                IF(film(p,q)>0)THEN
                   count=count+1
                ENDIF
             ENDDO
          ENDDO
       ENDIF
       IF(count>=40)THEN
          success=.TRUE.
          conti=.FALSE.
          EXIT outer
       ENDIF
     ENDDO final
   ENDDO outer


   WRITE(11,*)40
   WRITE(11,*)
   IF(success)THEN
      total=t_shape+l_shape+square
      DO b=2,6
         DO a=2,9
            WRITE(11,*)a-1,b-1,film(a,b)
         ENDDO
      ENDDO
      WRITE(*,*)"T-shape : ",t_shape
      WRITE(*,*)"L-shape : ",l_shape
      WRITE(*,*)"Square  : ",square
      WRITE(*,*)"TOTAL   : ",total
   ENDIF

STOP
END PROGRAM  com_third
