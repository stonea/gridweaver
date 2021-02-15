!module inter_environment
!    use mod_string
!    use mod_linkedList
!    use mod_io
!    implicit none
!    
!    external Environment
!    
!    interface
!        subroutine environment_new(self)
!            import Environment
!            type(Environment), intent(inout) :: self
!        end subroutine
!
!        subroutine environment_newFromFile(self, in)
!            import Environment, File
!            type(Environment), intent(inout) :: self
!            type(File), intent(inout) :: in
!        end subroutine
!
!        subroutine environment_free(self)
!            import Environment
!            type(Environment), intent(inout) :: self
!        end subroutine
!
!        function environment_newNeighbor(self)
!            import Neighbor, Environment
!            type(Neighbor), pointer :: environment_newNeighbor
!            type(Environment), intent(inout) :: self
!        end function
!
!        function environment_getNeighbor(self, id)
!            import Neighbor, Environment, String
!            type(Neighbor), pointer :: environment_getNeighbor
!            type(Environment), intent(in) :: self
!            type(String), intent(in)      :: id
!        end function
!
!        function environment_newSubGrid(self)
!            import SubGrid, Environment
!            type(SubGrid), pointer :: environment_newSubGrid
!            type(Environment), intent(inout) :: self
!        end function
!
!        function environment_getSubGrid(self, id)
!            import SubGrid, Environment, String
!            type(SubGrid), pointer :: environment_getSubGrid
!            type(Environment), intent(in) :: self
!            type(String), intent(in)      :: id
!        end function
!
!        function environment_newGrid(self)
!            import Grid, Environment
!            type(Grid), pointer :: environment_newGrid
!            type(Environment), intent(inout) :: self
!        end function
!
!        function environment_getGrid(self, id)
!            import Grid, Environment, String
!            type(Grid), pointer :: environment_getGrid
!            type(Environment), intent(in) :: self
!            type(String), intent(in)      :: id
!        end function
!
!        subroutine environment_print(self, out)
!            import Environment
!            type(Environment)   :: self
!            integer, intent(in) :: out
!        end subroutine
!
!        subroutine environment_printSimp(self, out)
!            import Environment
!            type(Environment)   :: self
!            integer, intent(in) :: out
!        end subroutine
!
!        subroutine environment_input(self, in)
!            import Environment
!            type(Environment)         :: self
!            type(File), intent(inout) :: in
!        end subroutine
!    end interface
!end module inter_environment
