case class UndoRedo[T](undoStack: List[T] = List.empty, redoStack: List[T] = List.empty) {
  def canUndo: Boolean = undoStack.nonEmpty

  def canRedo: Boolean = redoStack.nonEmpty

  def push(state: T): UndoRedo[T] = copy(undoStack = state :: undoStack, redoStack = List.empty)

  def undo(currentState: T): (T, UndoRedo[T]) = {
    val previousState :: restOfUndoStack = undoStack
    (previousState, UndoRedo(restOfUndoStack, currentState :: redoStack))
  }

  def redo(currentState: T): (T, UndoRedo[T]) = {
    val previousState :: restOfRedoStack = redoStack
    (previousState, UndoRedo(currentState :: undoStack, restOfRedoStack))
  }

}
