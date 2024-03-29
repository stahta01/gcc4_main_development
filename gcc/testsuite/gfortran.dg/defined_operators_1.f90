! { dg-do compile }
! { dg-options "-std=legacy" }
! Tests the fix for PR27122, in which the requirements of 12.3.2.1.1
! for defined operators were not enforced.
! 
! Based on PR test by Thomas Koenig  <tkoenig@gcc.gnu.org>
!
module mymod
  interface operator (.foo.)
     module procedure foo_0 ! { dg-error "must have at least one argument" }
     module procedure foo_1 ! { dg-error "must be INTENT" }
     module procedure foo_2 ! { dg-error "cannot be optional" }
     module procedure foo_3 ! { dg-error "must have, at most, two arguments" }
     module procedure foo_1_OK
     module procedure foo_2_OK
     function foo_chr (chr) ! { dg-error "cannot be assumed character length" }
       character(*) :: foo_chr
       character(*), intent(in) :: chr
     end function foo_chr
     subroutine bad_foo (chr) ! { dg-error "must be a FUNCTION" }
       character(*), intent(in) :: chr
     end subroutine bad_foo
  end interface
contains
  function foo_0 ()
    integer :: foo_1
    foo_0 = 1
  end function foo_0
  function foo_1 (a)
    integer :: foo_1
    integer :: a
    foo_1 = 1
  end function foo_1
  function foo_1_OK (a)
    integer :: foo_1_OK
    integer, intent (in) :: a
    foo_1_OK = 1
  end function foo_1_OK
  function foo_2 (a, b)
    integer :: foo_2
    integer, intent(in) :: a
    integer, intent(in), optional :: b
    foo_2 = 2 * a + b
  end function foo_2
  function foo_2_OK (a, b)
    real :: foo_2_OK
    real, intent(in) :: a
    real, intent(in) :: b
    foo_2_OK = 2.0 * a + b
  end function foo_2_OK
  function foo_3 (a, b, c)
    integer :: foo_3
    integer, intent(in) :: a, b, c
    foo_3 = a + 3 * b - c
  end function foo_3
end module mymod
! { dg-final { cleanup-modules "mymod" } }
