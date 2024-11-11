defmodule Boggle do

  @moduledoc """
    Add your boggle function below. You may add additional helper functions if you desire.
    Test your code by running 'mix test' from the tester_ex_simple directory.
  """

  def boggle(board, words) do
    len = tuple_size(board)
    # words = Enum.filter(words, fn str -> String.length(str) <= (len * len) end)
    trie = createTrie(words, len)
    len = tuple_size(board)
    m = runner(board, trie, len)
    finalAnswer(words, m, len)
  end


  def finalAnswer(words, m, len) do
    finalAnswer(words, m, %{}, len)
  end

  def finalAnswer([], _m, result, _max) do
    result
  end

  def finalAnswer([h | t], m, result, max) do

    if Map.has_key?(m,h) do
      finalAnswer(t, m , Map.put(result, h, Map.get(m, h)), max)
    else
      finalAnswer(t, m , result, max)
    end

  end

  def runner(board, trie, len) do
    runner(0,0, board, trie, %{}, len - 1)
  end

  def runner(len, len, board, trie, m, len) do
    dfs(board, {len, len}, [], m, "", trie)
  end

  def runner(row, len, board, trie, m, len) do
    r = dfs(board, {row, len}, [], m, "", trie)
    runner(row + 1,0, board, trie, r, len)
  end

  def runner(row, col, board, trie, m, len) when col < (len) do
    r = dfs(board, {row, col}, [], m, "", trie)
    runner(row,col + 1, board, trie, r , len)
  end

  def dfs(matrix, trie, startPosition) do
    dfs(matrix, startPosition, [], %{}, "", trie)
  end

  def dfs(matrix, currentPosition, visitedList, wordsToReturn, currentWord, trie) do
    if Map.has_key?(trie,currentWord) do

      if Enum.member?(visitedList, currentPosition) do

        wordsToReturn
      else

        col = elem(currentPosition, 1)
        row = elem(currentPosition, 0)

        len = tuple_size(matrix)

        newLetter = elem(elem(matrix, row), col)

        visitedList = visitedList ++ [currentPosition]

        currentWord = currentWord <> newLetter

        wordsToReturn = Map.put(wordsToReturn, currentWord, visitedList)

        possibleMoves = possibleMoves(currentPosition, len)

        #IO.puts "visited #{inspect visitedList} currentWord #{inspect currentWord} wordsToReturn #{inspect wordsToReturn} trie #{inspect trie}"

        c = Enum.reduce(possibleMoves, wordsToReturn, fn {row, col}, acc ->
          dfs(matrix, {row, col}, visitedList, acc, currentWord, trie)
        end)
        c
      end

    else
      wordsToReturn
    end

  end


  def possibleMoves(currentPosition, len) do
    moves = [{1,0},{-1,0},{0,1},{0,-1},{-1,-1},{1,-1},{-1,1},{1,1}]
    possibleMoves(currentPosition, moves, len - 1, [])
  end

  def possibleMoves(_currentPosition,[], _len, acc) do
    acc
  end

  def possibleMoves(currentPosition,[h | t], len, acc) do
    col = elem(currentPosition, 1)
    row = elem(currentPosition, 0)
    colt = elem(h, 1)
    rowt = elem(h, 0)
    if (col + colt) >= 0 and (col + colt) <= len and (row + rowt) >= 0 and (row + rowt) <= len do
      possibleMoves(currentPosition, t, len, [{row + rowt, col + colt} | acc])
    else
      possibleMoves(currentPosition, t, len, acc)
    end
  end

  def createTrie(words, len) do
    createTrie(words, %{"" => 1}, len)
  end

  def createTrie([h | t], m, len) do
    if String.length(h) > (len * len) do
      createTrie(t,m, len)
    else
      m = addWordToDict(h, m)
      createTrie(t, m, len)
    end

  end

  def createTrie([], m, _len) do
    m
  end

  def addWordToDict(word, d) do
    addWordToDict(word, d, 1, String.length(word))
  end

  def addWordToDict(word, d, l, l) do
    Map.put(d,String.slice(word,0, l),1)
  end

  def addWordToDict(word, d, s, l) do
    d = Map.put(d,String.slice(word,0,s),1)
    addWordToDict(word, d, s + 1, l)
  end

end
