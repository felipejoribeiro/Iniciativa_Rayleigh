!
! Example showing how to use OpenGL's feedback mode to capture
! transformed vertices and output them as Encapsulated PostScript.
! Handles limited hidden surface removal by sorting and does
! smooth shading (albeit limited due to PostScript).
!
! Modified for use with PHAML graphics, William F. Mitchell, 5/15/97
! Translated to Fortran by William F. Mitchell, 6/16/00
! Extended to an example for f90gl by William F. Mitchell, 1/3/03
!
! This is a translation of a C program written by Mark J. Kilgard.  The
! original C program had the following notice:
!
! /* Copyright (c) Mark J. Kilgard, 1997. */
! 
! /* This program is freely distributable without licensing fees 
!    and is provided without guarantee or warrantee expressed or 
!    implied. This program is -not- in the public domain. */
!
! (end of Mark's notice)
!
! The Fortran version is a contribution of NIST, not subject to copyright.
!
! The OpenGL image must be drawn in display list number 1, since this
! routine uses 'call glCallList(1_GLint)' to render the image.
!
! The interface for the public routine is:
!
! subroutine outputEPS(bsize, doSort, filename)
! integer, intent(in) :: bsize
! logical, intent(in) :: doSort
! character(len=*), intent(in), optional :: filename
!
! bsize is the amount of space (in words) to allocate for the feedback buffer.
! doSort indicates whether or not to sort the rendered objects, back to front.
! filename is the name of the file to which to write the postscript.  If it
! is not present, the feedback buffer is printed (for debugging)
!
module eps

  use opengl_gl
  use opengl_glu
  use opengl_glut

  implicit none
  private
  public :: outputEPS

  real(GLfloat) :: pointSize(1)

  character(len=74) :: gouraudtriangleEPS(25) = (/ &
    '/bd{bind def}bind def /triangle { aload pop   setrgbcolor  aload pop 5 3  ',&
    'roll 4 2 roll 3 2 roll exch moveto lineto lineto closepath fill } bd      ',&
    '/computediff1 { 2 copy sub abs threshold ge {pop pop pop true} { exch 2   ',&
    'index sub abs threshold ge { pop pop true} { sub abs threshold ge } ifelse',&
    '} ifelse } bd /computediff3 { 3 copy 0 get 3 1 roll 0 get 3 1 roll 0 get  ',&
    'computediff1 {true} { 3 copy 1 get 3 1 roll 1 get 3 1 roll 1 get          ',&
    'computediff1 {true} { 3 copy 2 get 3 1 roll  2 get 3 1 roll 2 get         ',&
    'computediff1 } ifelse } ifelse } bd /middlecolor { aload pop 4 -1 roll    ',&
    'aload pop 4 -1 roll add 2 div 5 1 roll 3 -1 roll add 2 div 3 1 roll add 2 ',&
    'div 3 1 roll exch 3 array astore } bd /gouraudtriangle { computediff3 { 4 ',&
    '-1 roll aload 7 1 roll 6 -1 roll pop 3 -1 roll pop add 2 div 3 1 roll add ',&
    '2 div exch 3 -1 roll aload 7 1 roll exch pop 4 -1 roll pop add 2 div 3 1  ',&
    'roll add 2 div exch 3 -1 roll aload 7 1 roll pop 3 -1 roll pop add 2 div 3',&
    '1 roll add 2 div exch 7 3 roll 10 -3 roll dup 3 index middlecolor 4 1 roll',&
    '2 copy middlecolor 4 1 roll 3 copy pop middlecolor 4 1 roll 13 -1 roll    ',&
    'aload pop 17 index 6 index 15 index 19 index 6 index 17 index 6 array     ',&
    'astore 10 index 10 index 14 index gouraudtriangle 17 index 5 index 17     ',&
    'index 19 index 5 index 19 index 6 array astore 10 index 9 index 13 index  ',&
    'gouraudtriangle 13 index 16 index 5 index 15 index 18 index 5 index 6     ',&
    'array astore 12 index 12 index 9 index gouraudtriangle 17 index 16 index  ',&
    '15 index 19 index 18 index 17 index 6 array astore 10 index 12 index 14   ',&
    'index gouraudtriangle 18 {pop} repeat } { aload pop 5 3 roll aload pop 7 3',&
    'roll aload pop 9 3 roll 4 index 6 index 4 index add add 3 div 10 1 roll 7 ',&
    'index 5 index 3 index add add 3 div 10 1 roll 6 index 4 index 2 index add ',&
    'add 3 div 10 1 roll 9 {pop} repeat 3 array astore triangle } ifelse } bd  ' &
    /)
  !
  ! OpenGL's GL_3D_COLOR feedback vertex format. 
  ! Use as offsets from base entry.
  !
  integer, parameter :: X=0, Y=1, Z=2, CRED=3, CGREEN=4, CBLUE=5, ALPHA=6

  type DepthIndex 
    integer(GLint) :: ptr
    real(GLfloat) :: depth
  end type DepthIndex

  interface operator(.le.)
    module procedure di_le
  end interface

  interface operator(.lt.)
    module procedure di_lt
  end interface

  interface operator(.gt.)
    module procedure di_gt
  end interface

  interface operator(.ge.)
    module procedure di_ge
  end interface

  contains

  subroutine print3DcolorVertex(count, buffer)

    integer(GLint), intent(inout) :: count
    real(GLfloat), intent(in) :: buffer(:)
    !
    ! Write contents of one vertex to stdout.
    !
    write(*,fmt='(2x,7(f5.2,1x))') buffer(count:count+6)
    count = count + 7

  end subroutine print3DcolorVertex
  subroutine printBuffer(bsize, buffer)

    integer, intent(in) :: bsize
    real(GLfloat),intent(in) :: buffer(:)

    integer(GLint) :: count
    integer(GLint) :: token
    integer :: nvertices, i

    count = 1
    do while (count < bsize)
      token = buffer(count)
      count = count + 1
      select case (token)
      case (int(GL_PASS_THROUGH_TOKEN))
        write(*,*) 'GL_PASS_THROUGH_TOKEN'
        write(*,fmt='(2x,f5.2)') buffer(count)
        count = count + 1
      case (int(GL_POINT_TOKEN))
        write(*,*) 'GL_POINT_TOKEN'
        call print3DcolorVertex(count, buffer)
      case (int(GL_LINE_TOKEN))
        write(*,*) 'GL_LINE_TOKEN'
        call print3DcolorVertex(count, buffer)
        call print3DcolorVertex(count, buffer)
      case (int(GL_LINE_RESET_TOKEN))
        write(*,*) 'GL_LINE_RESET_TOKEN'
        call print3DcolorVertex(count, buffer)
        call print3DcolorVertex(count, buffer)
      case (int(GL_POLYGON_TOKEN))
        write(*,*) 'GL_POLYGON_TOKEN'
        nvertices = buffer(count)
        count = count + 1
        do i=1,nvertices
          call print3DcolorVertex(count, buffer)
        end do
      case default
        write(*,*) & 
          'Incomplete implementation.  Unexpected token ',token
      end select
    end do
  end subroutine printBuffer
  subroutine spewPrimitiveEPS(file, loc, buffer)

    integer, intent(in) :: file
    integer, intent(inout) :: loc
    real(GLfloat), intent(in) :: buffer(:)

    integer(GLint) :: token
    integer :: nvertices, i
    real(GLfloat) :: red, green, blue
    logical :: smooth
    real(GLfloat) :: dx, dy, dr, dg, db, absR, absG, absB, colormax
    integer :: steps
    real(GLfloat) :: xstep, ystep, rstep, gstep, bstep
    real(GLfloat) :: xnext, ynext, rnext, gnext, bnext, distance
    !
    ! Lower for better smooth lines.
    !
    real(GLfloat), parameter :: EPS_SMOOTH_LINE_FACTOR = 0.06

    token = buffer(loc)
    loc = loc + 1
    select case (token)
    case (int(GL_LINE_RESET_TOKEN), int(GL_LINE_TOKEN))

      dr = buffer(loc+7+CRED) - buffer(loc+CRED)
      dg = buffer(loc+7+CGREEN) - buffer(loc+CGREEN)
      db = buffer(loc+7+CBLUE) - buffer(loc+CBLUE)

      if (dr /= 0 .or. dg /= 0 .or. db /= 0) then
        !
        ! Smooth shaded line.
        !
        dx = buffer(loc+7+X) - buffer(loc+X)
        dy = buffer(loc+7+Y) - buffer(loc+Y)
        distance = sqrt(dx * dx + dy * dy)

        absR = abs(dr)
        absG = abs(dg)
        absB = abs(db)

        colormax = max(absR, absG, absB)
        steps = max(1.0_GLfloat, colormax * distance * EPS_SMOOTH_LINE_FACTOR)

        xstep = dx / steps
        ystep = dy / steps

        rstep = dr / steps
        gstep = dg / steps
        bstep = db / steps

        xnext = buffer(loc+X)
        ynext = buffer(loc+Y)
        rnext = buffer(loc+CRED)
        gnext = buffer(loc+CGREEN)
        bnext = buffer(loc+CBLUE)
        !
        ! Back up half a step; we want the end points to be
        ! exactly the their endpoint colors.
        !
        xnext = xnext - xstep / 2.0_GLfloat
        ynext = ynext - ystep / 2.0_GLfloat
        rnext = rnext - rstep / 2.0_GLfloat
        gnext = gnext - gstep / 2.0_GLfloat
        bnext = bnext - bstep / 2.0_GLfloat
      else
        !
        ! Single color line.
        !
        steps = 0
      endif

      write(file,*) buffer(loc+CRED),buffer(loc+CGREEN), &
                    buffer(loc+CBLUE), ' setrgbcolor'
      write(file,*) buffer(loc+X),buffer(loc+Y),' moveto'

      do i=1,steps
        xnext = xnext + xstep
        ynext = ynext + ystep
        rnext = rnext + rstep
        gnext = gnext + gstep
        bnext = bnext + bstep
        write(file,*) xnext, ynext, ' lineto stroke'
        write(file,*) rnext, gnext, bnext, ' setrgbcolor'
        write(file,*) xnext, ynext, ' moveto'
      end do
      write(file,*) buffer(loc+7+X),buffer(loc+7+Y),' lineto stroke'
      ! 
      ! Each vertex element in the feedback buffer is 7 GLfloats.
      !
      loc = loc + 14  

    case (int(GL_POLYGON_TOKEN))
      nvertices = buffer(loc)
      loc = loc + 1

      if( nvertices > 0 )then
        red    = buffer(loc+CRED)
        green  = buffer(loc+CGREEN)
        blue   = buffer(loc+CBLUE)
        smooth = .false.
        do i=1,nvertices-1
          if( red   /= buffer(loc+7*i+CRED)   .or. &
              green /= buffer(loc+7*i+CGREEN) .or. &
              blue  /= buffer(loc+7*i+CBLUE) )then
            smooth = .true.
            exit
          endif
        enddo
        if( smooth )then
          !
          ! Smooth shaded polygon; varying colors at vetices.
          !
          ! Break polygon into 'nvertices-2' triangle fans.
          !
          do i=1,nvertices-2
            write(file,*) '[',buffer(loc+X),buffer(loc+7*i+X), &
               buffer(loc+7*(i+1)+X),buffer(loc+Y),buffer(loc+7*i+Y), &
               buffer(loc+7*(i+1)+Y),'] [',buffer(loc+CRED),buffer(loc+CGREEN), &
               buffer(loc+CBLUE),'] [',buffer(loc+7*i+CRED),buffer(loc+7*i+CGREEN), &
               buffer(loc+7*i+CBLUE),'] [',buffer(loc+7*(i+1)+CRED), &
               buffer(loc+7*(i+1)+CGREEN),buffer(loc+7*(i+1)+CBLUE), &
               '] gouraudtriangle'
          enddo
        else
          !
          ! Flat shaded polygon; all vertex colors the same.
          !
          write(file,*) 'newpath'
          write(file,*) red, green, blue, ' setrgbcolor'
          !
          ! Draw a filled triangle.
          !
          write(file,*) buffer(loc+X),buffer(loc+Y),' moveto'
          do i=1,nvertices-1
            write(file,*) buffer(loc+7*i+X),buffer(loc+7*i+Y),' lineto'
          end do
          write(file,*) 'closepath fill'
          write(file,*) ''
        endif
      endif
      !
      ! Each vertex element in the
      ! feedback buffer is 7 GLfloats.
      !
      loc = loc + nvertices * 7 
 
    case (int(GL_POINT_TOKEN))
      write(file,*) buffer(loc+CRED),buffer(loc+CGREEN),buffer(loc+CBLUE), &
                             ' setrgbcolor'
      write(file,*) buffer(loc+X),buffer(loc+Y),pointSize(1)/2.0,0, &
                             360,' arc fill'
      write(file,*) ''
      !
      ! Each vertex element in the feedback buffer is 7 GLfloats.
      !
      loc = loc + 7  

    case default
      write(*,*) 'Incomplete implementation.  Unexpected token ',token

    end select
  end subroutine spewPrimitiveEPS
  subroutine spewUnsortedFeedback(file, bsize, buffer)

    integer, intent(in) :: file, bsize
    real(GLfloat), intent(in) :: buffer(:)

    integer :: loc

    loc = 1
    do while( loc < bsize )
      call spewPrimitiveEPS(file, loc, buffer)
    end do
  end subroutine spewUnsortedFeedback
  subroutine spewSortedFeedback(file, bsize, buffer)

    integer, intent(in) :: file, bsize
    real(GLfloat), intent(in) :: buffer(:)

    integer :: token
    integer :: loc
    real(GLfloat) :: depthSum
    integer :: nprimitives, item
    type(DepthIndex), allocatable :: prims(:)
    integer :: nvertices, i
    real :: ydum(1)

    !
    ! Count how many primitives there are.
    !
    nprimitives = 0
    loc = 1
    do while( loc < bsize )
      token = buffer(loc)
      loc = loc + 1
      select case (token)
      case(int(GL_LINE_TOKEN), int(GL_LINE_RESET_TOKEN))
        loc = loc + 14
        nprimitives = nprimitives + 1
      case(int(GL_POLYGON_TOKEN))
        nvertices = buffer(loc)
        loc = loc + 1
        loc = loc + 7*nvertices
        nprimitives = nprimitives + 1
      case(int(GL_POINT_TOKEN))
        loc = loc + 7
        nprimitives = nprimitives + 1
      case default
        write(*,*) 'Incomplete implementation.  Unexpected token ',token
      end select
    enddo

    !
    ! Allocate an array of pointers that will point back at
    ! primitives in the feedback buffer.  There will be one
    ! entry per primitive.  This array is also where we keep the
    ! primitive's average depth.  There is one entry per
    ! primitive  in the feedback buffer.
    !
    allocate(prims(nprimitives))

    item = 1
    loc = 1
    do while (loc < bsize)
      !
      ! Save this primitive's location.
      !
      prims(item)%ptr = loc  
      token = buffer(loc)
      loc = loc + 1
      select case (token)
      case(int(GL_LINE_TOKEN), int(GL_LINE_RESET_TOKEN))
        depthSum = buffer(loc+Z) + buffer(loc+7+Z)
        prims(item)%depth = depthSum / 2.0
        !
        ! WFM to force triangle edges on top of filled triangle
        ! prims(item)%depth = prims(item)%depth - .0001
        !
        loc = loc + 14
      case(int(GL_POLYGON_TOKEN))
        nvertices = buffer(loc)
        loc = loc + 1
        depthSum = buffer(loc+Z)
        do i=1,nvertices-1
          depthSum = depthSum + buffer(loc+7*i+Z)
        enddo
        prims(item)%depth = depthSum / nvertices
        loc = loc + 7*nvertices
      case(int(GL_POINT_TOKEN))
        prims(item)%depth = buffer(loc+Z)
        loc = loc + 7
      case default
        write(*,*) 'Incomplete implementation.  Unexpected token ',token
      end select
      item = item + 1
    end do

    !
    !  Sort the primitives back to front.
    !
    call ssort(prims, ydum, nprimitives, -1)
    !
    !    XXX Understand that sorting by a primitives average depth
    !    doesn't allow us to disambiguate some cases like self
    !    intersecting polygons.  Handling these cases would require
    !    breaking up the primitives.  That's too involved for this
    !    example.  Sorting by depth is good enough for lots of
    !    applications.
    !
    !  Emit the Encapsulated PostScript for the primitives in
    !  back to front order.
    !
    do item=1,nprimitives
      call spewPrimitiveEPS(file, prims(item)%ptr, buffer)
    end do

    deallocate(prims)
  
  end subroutine spewSortedFeedback
  subroutine spewWireFrameEPS(file, doSort, bsize, buffer, creator)

    integer, intent(in) :: file, bsize
    logical, intent(in) :: doSort
    real(GLfloat), intent(in) :: buffer(:)
    character(len=*), intent(in) :: creator

    real(GLfloat) :: clearColor(4), viewport(4)
    real(GLfloat) :: lineWidth(1)
    integer :: i
    !
    ! Lower for better (slower) smooth shading.
    !
    real(GLfloat), parameter :: EPS_GOURAUD_THRESHOLD=0.1

    !
    ! Read back a bunch of OpenGL state to help make the EPS
    ! consistent with the OpenGL clear color, line width, point
    ! bsize, and viewport.
    !
    call glGetFloatv(GL_VIEWPORT, viewport)
    call glGetFloatv(GL_COLOR_CLEAR_VALUE, clearColor)
    call glGetFloatv(GL_LINE_WIDTH, lineWidth)
    call glGetFloatv(GL_POINT_SIZE, pointSize)

    !
    !    Emit EPS header. */
    !
    write(file,'(a)') '%!PS-Adobe-2.0 EPSF-2.0'
    write(file,'(a)') '%%Creator: eps.f90 (using OpenGL feedback)'
    write(file,'(a,4i10)') '%%BoundingBox: ', int(viewport)
    write(file,'(a)') '%%EndComments'
    write(file,*) ''
    write(file,*) 'gsave'
    write(file,*) ''

    !
    ! Output Frederic Delhoume's 'gouraudtriangle' PostScript fragment.
    !
    write(file,*) '% the gouraudtriangle PostScript fragement below is free'
    write(file,*) '% written by Frederic Delhoume (delhoume@ilog.fr)'
    write(file,*) '/threshold ',EPS_GOURAUD_THRESHOLD,' def'
    do i=1,size(gouraudtriangleEPS)
      write(file,*) gouraudtriangleEPS(i)
    end do

    write(file,*) ''
    write(file,*) lineWidth(1),' setlinewidth'

    !
    ! Clear the background like OpenGL had it.
    !
    write(file,*) clearColor(1:3),' setrgbcolor'
    write(file,*) viewport,' rectfill'
    write(file,*) ''

    if( doSort )then
      call spewSortedFeedback(file, bsize, buffer)
    else
      call spewUnsortedFeedback(file, bsize, buffer)
    endif

    !
    !   Emit EPS trailer.
    !
    write(file,*) 'grestore'
    write(file,*) ''
    !
    ! WFM I don't know why Mark put this comment instead of showpage, unless some
    ! apps that take eps input don't like it (ghostview is fine with it)
    ! write(file,*) '%Add `showpage' to the end of this file to be able to print to a printer.'
    !
    write(file,*) '% ''showpage'' is needed to print to a printer, but there'
    write(file,*) '% may be some apps for which it must be removed'
    write(file,*) 'showpage'

    close(file)
  
  end subroutine spewWireFrameEPS
  subroutine outputEPS(bsize, doSort, filename)
    
    integer, intent(in) :: bsize
    logical, intent(in) :: doSort
    character(len=*), intent(in), optional :: filename

    real(GLfloat), allocatable, target :: feedbackBuffer(:)
    integer(GLint) :: returned, idum
    integer :: file, iostat
    logical :: opened

    allocate(feedbackBuffer(bsize))
    call glFeedbackBuffer(bsize, GL_3D_COLOR, c_loc(feedbackBuffer))
    idum = glRenderMode(GL_FEEDBACK)
    call glCallList(1_GLint)
    returned = glRenderMode(GL_RENDER)
    if( present(filename) )then
      file = 11
      do
        inquire(file,opened=opened)
        if( .not. opened )exit
        file = file + 1
      enddo
      open(file,file=filename,iostat=iostat)
      if( iostat == 0 )then
        call spewWireFrameEPS(file, doSort, returned, feedbackBuffer, 'rendereps')
      else
        write(*,*) 'Could not open ', filename
      endif
    else
      !
      ! Helps debugging to be able to see the decode 
      ! feedback buffer as text.
      !
      call printBuffer(returned, feedbackBuffer)
    endif
    
    deallocate(feedbackBuffer)
  
  end subroutine outputEPS
  !
  ! The following routine was taken from CMLIB at http://gams.nist.gov
  ! and modified to be acceptable in free format, not require subroutine
  ! xerror, and to sort type(DepthIndex).
  ! Later made 3 trivial changes to remove f95 obsolescent features
  !
  subroutine ssort(x,y,n,kflag)
  !***BEGIN PROLOGUE  SSORT
  !***DATE WRITTEN   761101   (YYMMDD)
  !***REVISION DATE  820801   (YYMMDD)
  !***CATEGORY NO.  N6A2B1
  !***KEYWORDS  QUICKSORT,SINGLETON QUICKSORT,SORT,SORTING
  !***AUTHOR  JONES, R. E., (SNLA)
  !           WISNIEWSKI, J. A., (SNLA)
  !***PURPOSE  SSORT sorts array X and optionally makes the same
  !            interchanges in array Y.  The array X may be sorted in
  !            increasing order or decreasing order.  A slightly modified
  !            QUICKSORT algorithm is used.
  !***DESCRIPTION
  !
  !   Written by Rondall E. Jones
  !   Modified by John A. Wisniewski to use the Singleton quicksort
  !   algorithm.  Date 18 November 1976.
  !
  !   Abstract
  !       SSORT sorts array X and optionally makes the same
  !       interchanges in array Y.  The array X may be sorted in
  !       increasing order or decreasing order.  A slightly modified
  !       quicksort algorithm is used.
  !
  !   Reference
  !       Singleton, R. C., Algorithm 347, An Efficient Algorithm for
  !       Sorting with Minimal Storage, CACM,12(3),1969,185-7.
  !
  !   Description of Parameters
  !       X - array of values to be sorted   (usually abscissas)
  !       Y - array to be (optionally) carried along
  !       N - number of values in array X to be sorted
  !       KFLAG - control parameter
  !           =2  means sort X in increasing order and carry Y along.
  !           =1  means sort X in increasing order (ignoring Y)
  !           =-1 means sort X in decreasing order (ignoring Y)
  !           =-2 means sort X in decreasing order and carry Y along.
  !
  !***REFERENCES  SINGLETON,R.C., ALGORITHM 347, AN EFFICIENT ALGORITHM
  !                 FOR SORTING WITH MINIMAL STORAGE, CACM,12(3),1969,
  !                 185-7.
  !***ROUTINES CALLED  XERROR
  !***END PROLOGUE  SSORT
  !
    integer n
    real Y(*)
    integer IL(21),IU(21)
    type(DepthIndex) :: X(N), T, TT
    integer :: m, j, k, ij, l, nn, kk, kflag, i
    real :: r, ty, tty
    !
    ! first executable statement  ssort
    !
    NN = N
    IF( NN .GE. 1 ) GO TO 10
    !
    ! CALL XERROR ( 'SSORT- THE NUMBER OF VALUES TO BE SORTED 
    ! WAS NOT POSITIVE.',58,1,1)
    !
    write(*,*) 'SSORT- THE NUMBER OF VALUES TO BE SORTED WAS NOT POSITIVE.'
    return
 10 KK = IABS(KFLAG)
    IF ((KK.EQ.1).OR.(KK.EQ.2)) GO TO 15
    !
    ! CALL XERROR ( 'SSORT- THE SORT CONTROL PARAMETER, K, 
    ! WAS NOT 2, 1, -1, OR -2.',62,2,1)
    !
    write(*,*) 'SSORT- THE SORT CONTROL PARAMETER, K, WAS NOT 2, 1, -1, OR -2.'
    RETURN
    !
    ! ALTER ARRAY X TO GET DECREASING ORDER IF NEEDED
    !
 15 IF (KFLAG.GE.1) GO TO 30

    DO 20 I=1,NN
      X(I)%depth = -X(I)%depth
 20 CONTINUE
    ! WFM replace obsolete computed goto
    !   30 GO TO (100,200),KK
 30 IF (KK .EQ. 1) GOTO 100
    IF (KK .EQ. 2) GOTO 200
    !
    ! SORT X ONLY
    !
100 CONTINUE
    M=1
    I=1
    J=NN
    R=.375
110 IF (I .EQ. J) GO TO 155
115 IF (R .GT. .5898437) GO TO 120
    R=R+3.90625E-2
    GO TO 125
120 R=R-.21875
125 K=I
    !
    ! select a central element of the
    ! array and save it in location t
    !
    IJ = I + IFIX (FLOAT (J-I) * R)
    T=X(IJ)
    !
    ! if first element of array is greater
    ! than t, interchange with t
    !
      IF (X(I) .LE. T) GO TO 130
      X(IJ)=X(I)
      X(I)=T
      T=X(IJ)
  130 L=J
!                                  IF LAST ELEMENT OF ARRAY IS LESS THAN
!                                  T, INTERCHANGE WITH T
      IF (X(J) .GE. T) GO TO 140
      X(IJ)=X(J)
      X(J)=T
      T=X(IJ)
!                                  IF FIRST ELEMENT OF ARRAY IS GREATER
!                                  THAN T, INTERCHANGE WITH T
      IF (X(I) .LE. T) GO TO 140
      X(IJ)=X(I)
      X(I)=T
      T=X(IJ)
      GO TO 140
  135 TT=X(L)
      X(L)=X(K)
      X(K)=TT
!                                  FIND AN ELEMENT IN THE SECOND HALF OF
!                                  THE ARRAY WHICH IS SMALLER THAN T
  140 L=L-1
      IF (X(L) .GT. T) GO TO 140
!                                  FIND AN ELEMENT IN THE FIRST HALF OF
!                                  THE ARRAY WHICH IS GREATER THAN T
  145 K=K+1
        IF (X(K) .LT. T) GO TO 145
  !                                  INTERCHANGE THESE ELEMENTS
        IF (K .LE. L) GO TO 135
  !                                  SAVE UPPER AND LOWER SUBSCRIPTS OF
  !                                  THE ARRAY YET TO BE SORTED
        IF (L-I .LE. J-K) GO TO 150
        IL(M)=I
        IU(M)=L
        I=K
        M=M+1
        GO TO 160
    150 IL(M)=K
        IU(M)=J
        J=L
        M=M+1
        GO TO 160
  !                                  BEGIN AGAIN ON ANOTHER PORTION OF
  !                                  THE UNSORTED ARRAY
    155 M=M-1
        IF (M .EQ. 0) GO TO 300
        I=IL(M)
        J=IU(M)
    160 IF (J-I .GE. 1) GO TO 125
        IF (I .EQ. 1) GO TO 110
        I=I-1
    165 I=I+1
        IF (I .EQ. J) GO TO 155
        T=X(I+1)
        IF (X(I) .LE. T) GO TO 165
        K=I
    170 X(K+1)=X(K)
        K=K-1
        IF (T .LT. X(K)) GO TO 170
        X(K+1)=T
        GO TO 165
  !
  ! SORT X AND CARRY Y ALONG
  !
    200 CONTINUE
        M=1
        I=1
        J=NN
        R=.375
    210 IF (I .EQ. J) GO TO 255
    215 IF (R .GT. .5898437) GO TO 220
        R=R+3.90625E-2
        GO TO 225
    220 R=R-.21875
    225 K=I
  !                                  SELECT A CENTRAL ELEMENT OF THE
  !                                  ARRAY AND SAVE IT IN LOCATION T
        IJ = I + IFIX (FLOAT (J-I) *R)
        T=X(IJ)
        TY= Y(IJ)
  !                                  IF FIRST ELEMENT OF ARRAY IS GREATER
  !                                  THAN T, INTERCHANGE WITH T
        IF (X(I) .LE. T) GO TO 230
        X(IJ)=X(I)
        X(I)=T
        T=X(IJ)
         Y(IJ)= Y(I)
         Y(I)=TY
        TY= Y(IJ)
    230 L=J
  !                                  IF LAST ELEMENT OF ARRAY IS LESS THAN
  !                                  T, INTERCHANGE WITH T
        IF (X(J) .GE. T) GO TO 240
        X(IJ)=X(J)
        X(J)=T
        T=X(IJ)
         Y(IJ)= Y(J)
         Y(J)=TY
        TY= Y(IJ)
  !                                  IF FIRST ELEMENT OF ARRAY IS GREATER
  !                                  THAN T, INTERCHANGE WITH T
        IF (X(I) .LE. T) GO TO 240
        X(IJ)=X(I)
        X(I)=T
        T=X(IJ)
         Y(IJ)= Y(I)
         Y(I)=TY
        TY= Y(IJ)
        GO TO 240
    235 TT=X(L)
        X(L)=X(K)
        X(K)=TT
        TTY= Y(L)
         Y(L)= Y(K)
         Y(K)=TTY
  !                                  FIND AN ELEMENT IN THE SECOND HALF OF
  !                                  THE ARRAY WHICH IS SMALLER THAN T
    240 L=L-1
        IF (X(L) .GT. T) GO TO 240
  !                                  FIND AN ELEMENT IN THE FIRST HALF OF
  !                                  THE ARRAY WHICH IS GREATER THAN T
    245 K=K+1
        IF (X(K) .LT. T) GO TO 245
  !                                  INTERCHANGE THESE ELEMENTS
        IF (K .LE. L) GO TO 235
  !                                  SAVE UPPER AND LOWER SUBSCRIPTS OF
  !                                  THE ARRAY YET TO BE SORTED
        IF (L-I .LE. J-K) GO TO 250
        IL(M)=I
        IU(M)=L
        I=K
        M=M+1
        GO TO 260
    250 IL(M)=K
        IU(M)=J
        J=L
        M=M+1
        GO TO 260
  !                                  BEGIN AGAIN ON ANOTHER PORTION OF
  !                                  THE UNSORTED ARRAY
    255 M=M-1
        IF (M .EQ. 0) GO TO 300
        I=IL(M)
        J=IU(M)
    260 IF (J-I .GE. 1) GO TO 225
        IF (I .EQ. 1) GO TO 210
        I=I-1
    265 I=I+1
        IF (I .EQ. J) GO TO 255
        T=X(I+1)
        TY= Y(I+1)
        IF (X(I) .LE. T) GO TO 265
        K=I
    270 X(K+1)=X(K)
         Y(K+1)= Y(K)
        K=K-1
        IF (T .LT. X(K)) GO TO 270
        X(K+1)=T
         Y(K+1)=TY
        GO TO 265
  !
  ! CLEAN UP
  !
    300 IF (KFLAG.GE.1) RETURN
        DO 310 I=1,NN
        X(I)%depth = -X(I)%depth
    310 CONTINUE
        RETURN
        END subroutine ssort

  !
  ! comparison operators for SSORT
  !
  logical function di_le(a,b)
    type(DepthIndex), intent(in) :: a,b
    di_le = a%depth <= b%depth
  end function di_le

  logical function di_lt(a,b)
    type(DepthIndex), intent(in) :: a,b
    di_lt = a%depth < b%depth
  end function di_lt

  logical function di_gt(a,b)
    type(DepthIndex), intent(in) :: a,b
    di_gt = a%depth > b%depth
  end function di_gt

  logical function di_ge(a,b)
    type(DepthIndex), intent(in) :: a,b
    di_ge = a%depth >= b%depth
  end function di_ge

  end module eps
  !
  !-----------------------------------------------------------
  !
  ! Use the fscene.f90 program to illustrate use of rendereps.  
  ! Contrast the remainder of this file to the file fscene.f90.
  !
  ! GLUT Fortran program to render simple red scene.
  !
  ! This is a fortran 90 program in fixed source form.
  ! In a risky move, this program assumes that the default kind of
  ! integer is the same as glint and glsizei and also that the
  ! default real is the same kind as glfloat
  !
  subroutine display() bind(C)
    
    use opengl_gl
    use opengl_glu
    use opengl_glut

    integer(4) :: type=GL_COLOR_BUFFER_BIT + GL_DEPTH_BUFFER_BIT

    if( glIsList(1_gluint) == 1 ) call gldeletelists(1_gluint, 1_glsizei)
    !
    ! put the display in list 1 for rendereps
    !
    call glnewlist(1_gluint, gl_compile_and_execute)
    call glclear(type)
    call glpushmatrix()
    call glscalef(1.3, 1.3, 1.3)
    call glrotatef(20.0, 1.0, 0.0, 0.0)

    call glpushmatrix()
    call gltranslatef(-0.75, 0.5, 0.0)
    call glrotatef(90.0, 1.0, 0.0, 0.0)
    call glutsolidtorus(0.275_gldouble, 0.85_gldouble, 10, 15)
    call glpopmatrix()

    call glpushmatrix()
    call gltranslatef(-0.75, -0.5, 0.0)
    call glrotatef(270.0, 1.0, 0.0, 0.0)
    call glutsolidtetrahedron
    call glpopmatrix()

    call glpushmatrix()
    call gltranslatef(0.75, 0.0, -1.0)
    call glutsolidicosahedron
    call glpopmatrix()

    call glpopmatrix()
    call glendlist()
    call glutSwapBuffers()
  
  end
  subroutine reshape(w,h) bind(C)

    use opengl_gl
    use opengl_glu
    use opengl_glut

    integer(glint), intent(in), value ::  w,h
    real(gldouble) wr,hr, left,right,bottom,top
    double precision d
    
    call glviewport(0, 0, w, h)
    call glmatrixmode(GL_PROJECTION)
    call glloadidentity
    wr = w
    hr = h
    d = 1.0d0
    if( w <= h )then
      bottom=-2.5_gldouble * hr/wr
      top=2.5_gldouble * hr/wr
      call glortho(-2.5_gldouble, 2.5_gldouble,      &
                    bottom, top, &
                   -10.0_gldouble, 10.0_gldouble)
    else
      left=-2.5_gldouble * hr/wr
      right=2.5_gldouble * hr/wr
      call glortho(left, right,                    &
                    -2.5_gldouble,  2.5_gldouble,  &
                   -10.0_gldouble, 10.0_gldouble)
    endif
    
    call glmatrixmode(GL_MODELVIEW)
    
  end
  subroutine submenu(value) bind(C)

    use opengl_gl
    use opengl_glu
    use opengl_glut

    integer(glint), intent(in), value :: value

    if( value == 1 )then
      call glenable(GL_DEPTH_TEST)
      call glenable(GL_LIGHTING)
      call gldisable(GL_BLEND)
      call glpolygonmode(GL_FRONT_AND_BACK, GL_FILL)
    else
      call gldisable(GL_DEPTH_TEST)
      call gldisable(GL_LIGHTING)
      call glcolor3f(1.0, 1.0, 1.0)
      call glpolygonmode(GL_FRONT_AND_BACK, GL_LINE)
      call glenable(GL_LINE_SMOOTH)
      call glenable(GL_BLEND)
      call glblendfunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
    endif
    
    call glutpostredisplay
    
  end
  subroutine mainmenu(value) bind(C)

    use opengl_gl
    use opengl_glu
    use opengl_glut

    use eps

    integer(glint), intent(in), value :: value

    if( value == 10 )then
      call glutsetcursor(glut_cursor_wait)
      call outputeps(100000,.true.,'epsout.eps')
      call glutsetcursor(glut_cursor_inherit)
    endif
    
    if( value == 666 ) stop
    
  end
  program main

    use opengl_gl
    use opengl_glu
    use opengl_glut

    real lambient(4), ldiffuse(4), lspecular(4), lposition(4)

    data lambient  /0.0, 0.0, 0.0, 1.0/
    data ldiffuse  /1.0, 0.0, 0.0, 1.0/
    data lspecular /1.0, 1.0, 1.0, 1.0/
    data lposition /1.0, 1.0, 1.0, 0.0/

    interface
      subroutine display() bind(C)
      end subroutine display
      subroutine reshape(w,h) bind(C)
        use opengl_gl
        integer(glint), intent(inout):: w,h
      end subroutine reshape
      subroutine submenu(value) bind(C)
        use opengl_gl
        integer(glint), intent(in), value:: value
      end subroutine submenu
      subroutine mainmenu(value) bind(C)
        use opengl_gl
        integer(glint), intent(in), value:: value
      end subroutine mainmenu
    end interface

    call glutinit()
    call glutinitwindowposition(100,100)
    call glutinitwindowsize(500,500)

    call glutinitdisplaymode(GLUT_DOUBLE+GLUT_RGB+GLUT_DEPTH)
    i = glutcreatewindow('Fortran GLUT program'//char(0))
    !
    !--------------------------------------------------------
    !
    call gllightfv(GL_LIGHT0, GL_AMBIENT, lambient)
    call gllightfv(GL_LIGHT0, GL_DIFFUSE, ldiffuse)
    call gllightfv(GL_LIGHT0, GL_SPECULAR, lspecular)
    call gllightfv(GL_LIGHT0, GL_POSITION, lposition)
    call glenable(GL_LIGHT0)
    call gldepthfunc(GL_LESS)
    call glenable(GL_DEPTH_TEST)
    call glenable(GL_LIGHTING)
    !
    !--------------------------------------------------------
    !
    call glutdisplayfunc(display)
    call glutreshapefunc(reshape)
    i = glutcreatemenu(submenu)
    call glutaddmenuentry('Filled'//char(0), 1)
    call glutaddmenuentry('Outline'//char(0), 2)
    j = glutcreatemenu(mainmenu)
    call glutaddsubmenu('Polygon mode'//char(0), i)
    call glutaddmenuentry('Save postscript'//char(0),10)
    call glutaddmenuentry('Quit'//char(0), 666)
    call glutattachmenu(GLUT_RIGHT_BUTTON)
    call glutmainloop()
    
  end
