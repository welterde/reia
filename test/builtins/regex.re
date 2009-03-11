#
# RegexTest: Tests for Reia's regular expression type
# Copyright (C)2008 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

module RegexTest
  def run
    [
      #match_all(),
      #split(),
      #sub(),
      #gsub(),
      match_one(), 
      to_string(), 
      inspect()
    ]
  end
  
  def match_one
    TestHelper.expect(Regex, "match a single occurence", fun do
      string = "(a,b,c,d,e)"
      ("a,", /.,/.match(string))
    end)
  end
  
  def match_all
    TestHelper.expect(Regex, "match all occurencies", fun do
      string = "(a,b,c,d,e)"
      (["a,","b,","c,","d,"], /.,/.matches(string))
    end)
  end
  
  def split
    TestHelper.expect(Regex, "splits a string", fun do
      string = "a,b,c,d,e"
      (["a","b","c","d","e"], /,/.split(string))
    end)
  end
  
  def sub
    TestHelper.expect(Regex, "substitution", fun do
      string = "a,b,c,d,e"
      ("a12b,c,d,e", /,/.sub(string, "12"))
    end)
  end
  
  def gsub
    TestHelper.expect(Regex, "multiple substitution", fun do
      string = "a,b,c,d,e"
      ("a12b12c12d12e", /,/.gsub(string, "12"))
    end)
  end
  
  # converts to a string
  def to_string
    TestHelper.expect(Regex, "converts to a string", fun do
      ("/.,abc/", /.,abc/.inspect())
    end)
  end
  
  def inspect
    TestHelper.expect(Regex, "inspect returns valid regexp syntax", fun do
      ("/.,abc/", /.,abc/.inspect())
    end)
  end
end