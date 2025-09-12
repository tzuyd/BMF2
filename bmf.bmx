Rem

------------------------------
BMF - Blitzmax code formatter
------------------------------

 - Created by:	Henri Vihonen
 - Updated by:	Hotcakes
 - Version:		0.7.2 Beta

 * Description:

	Standalone code formatter For Blitzmax language that can be used by a code editor/IDE.
	BMF is a command line tool that can be used To format code files, Or format code lines in real time.

 -Usage: See example folder

EndRem

SuperStrict

Framework brl.standardio
Import brl.system
Import brl.linkedlist
Import brl.threads
Import brl.stringbuilder

Import brl.systemdefault
Import brl.ramstream

'Start from command line
If AppArgs And AppArgs.length > 1 Then
	
	Local c:TBMaxCode = New TBMaxCode
	
	Local in:Int, out:Int, s:String, v:Int

	For Local arg:String = EachIn AppArgs
		
		arg = arg.Trim()	'.ToUpper()
		
		If arg.StartsWith("-" ) Then
			
			s = arg.Replace( " ", "" ).Replace( "-", "" ).ToUpper()
			
			If s.StartsWith("STDIN" ) Then c.optStdIn = True
			If s.StartsWith("STDOUT" ) Then c.optStdOut = True
			If s.StartsWith("STDIO" ) Then c.optStdOut = True; c.optStdIn = True
			If s.StartsWith("I" ) Then in = True; out = False
			If s.StartsWith("O" ) Then in = False; out = True
			If s.StartsWith("V" ) Then v = True
			
		Else
			
			If in Then
				c.inUrl = arg
			ElseIf out
				c.outUrl = arg
			EndIf
		EndIf
		
	Next

	If v Then Print "Setting keywords.."
	c.SetKeywords()
	
	'Format a file
	'--------------
	If c.inUrl Then
		
		If v Then Print "Loading text.."
		
		Local text:String = LoadText( c.inUrl )
		If text Then
			
			text = c._parse(text)
			
			If c.outUrl Then
				SaveText( text, c.outUrl )
			Else
				SaveText( text, c.inUrl )
			EndIf
			
			If v Then
				Print "Saving text:"
				Print text + "~n"
			EndIf
		EndIf
	EndIf
	
	'Set to I/O mode 
	'----------------
	If c.optStdIn Then
	
		If v Then Print "Setting I/O mode active.."
		c.SetActive()
	EndIf
	
	If v Then Print "Ending.."
Else
	'Print "No args!"
EndIf 


Type TBMaxCode
	
	Const STYLE_KEYWORDS_1:Int = 1
	Const STYLE_KEYWORDS_2:Int = 2
	
	Global _autoKeywords:String					' Used for autocompletion
	Global _keywords1:String, _keywords2:String
	
	Global _traceType:String
	Global _wordList:TList = New TList

	Field _isMatch:Int, _isTraceable:Int
	
	Field inUrl:String, outUrl:String
	
	Field optAutoCap:Int = True
	Field optStdOut:Int
	Field optStdIn:Int
	
	Global mWrite:TMutex = TMutex.Create()
	
	Function GetAutoKeywords:String()
		Return _autoKeywords
	EndFunction
		
	Function GetKeywords:String( style:Int = STYLE_KEYWORDS_1 )
		If style = STYLE_KEYWORDS_2 Then
			Return _keywords2
		Else
			Return _keywords1
		EndIf
	EndFunction
	
	Method GetWord:String( key:String )
		If Not key Then Return key
		
		Local tmpKey:String = key.tolower()
		
		For Local w:TWord = EachIn _wordList
			If w.key = tmpKey Then
				_isMatch = True
				If w.trace Then
					_isTraceable = True
					_traceType = tmpKey
				Else
					_isTraceable = False
					_traceType = ""
				EndIf
				
				Return w.name
			EndIf
		Next
		
		_isMatch = False
		_isTraceable = False
		
		Return key
	EndMethod
	
	Function InitKeywords()
	
		If Not _wordList.count() Then Return
		_wordList.sort(False)
		
		_autoKeywords = ""
		_keywords1 = ""
		_keywords2 = ""
		
		For Local w:TWord = EachIn _wordList
			
			If Not w.key Then Continue
			
			If _autoKeywords Then _autoKeywords:+" "
			_autoKeywords:+w.key
			
			If w.style = STYLE_KEYWORDS_2
				If _keywords2 Then _keywords2:+" "
				_keywords2:+w.key
			Else
				If _keywords1 Then _keywords1:+" "
				_keywords1:+w.key
			EndIf
		Next
	EndFunction
	
	Function LoadKeywords:Int( words:String, style:Int = STYLE_KEYWORDS_1 )

		If Not words Then Notify "Error: Keywords not found.", True; Return False
		
		Local w:TWord
		Local ar:String[] = words.split(" " )
		
		If Not ar Then Notify "Error: Could not create keywords.", True; Return False
		
		For Local i:Int = 0 Until ar.length
			
			If Not ar[i].Trim() Then Continue
			
			w = New TWord
			w.style = style
			w.name = ar[i]
			w.key = ar[i].tolower()
			
			_wordList.addlast(w)
		Next
		
		Return True
	EndFunction
	
	Method SetActive()
		
		Local str:TBMFStream = TBMFStream.Create()
		Local s:String
		Local txt:String
		
		Try
			Repeat

				s = str.ReadString( 1 )
				str.text.append(s)
			Forever
		
		Catch o:Object

			txt = str.text.ToString()
			WriteStdout( _parse(txt) )
		EndTry
	EndMethod
	
	Method SetKeywords()
		
		Local keywords_1:String, keywords_2:String, traceWords:String
		
		Local Text$

		keywords_1 = "Abs Abstract Alias And Asc Assert Case Catch Chr Const Continue DebugLog DebugStop Default DefData Delete EachIn " + ..
					"Else ElseIf End EndExtern EndEnum EndFunction EndIf EndMethod EndRem EndSelect EndTry EndType Enum Exit Extends Extern " + ..
					"False Field Final For Forever Framework Function Global Goto If Import Incbin IncbinLen IncbinPtr Include " + ..
					"Len Local Max Method Min Mod Module ModuleInfo New Next Not Null Object Or Pi Print Private Public ReadData " + ..
					"Release Rem Repeat RestoreData Return Sar Select Self Sgn Shl Shr SizeOf Step Strict Super SuperStrict Then Throw " + ..
					"To True Try Type Until Var VarPtr Wend While"
					
		keywords_2 = "Byte Double Float Int Long Ptr Short String"
		
		traceWords = "Function Type Method Enum"
	
		Try
			Text = CacheAndLoadText("../../docs/html/Modules/commands.txt" )
		Catch exception:Object
		EndTry
		If Not Text Then
			LoadKeywords(keywords_1, STYLE_KEYWORDS_1)
		Else
			loadkeywords( LoadCommandsTxt( Text  ), STYLE_KEYWORDS_1  )
		EndIf 
		LoadKeywords(keywords_2, STYLE_KEYWORDS_2)
		SetTraceWords(traceWords)
		initKeywords()
		
	EndMethod
	
	Function LoadCommandsTxt$( Text$ )
		Local	i:Int, c:Int
		Local	token$
		Local sb:TStringBuilder = New TStringBuilder
		For Local l$ = EachIn Text.Split("~n" )
			For i = 0 Until l.Length
				c = l[i]
				If c = Asc( "_" ) Continue
				If c >= Asc( "0" ) And c <= Asc( "9" ) Continue
				If c >= Asc( "a" ) And c <= Asc( "z" ) Continue
				If c >= Asc( "A" ) And c <= Asc( "Z" ) Continue
				Exit
			Next
			token$ = l[..i]
			Select token
				Case "Byte", "Double", "Float", "Int", "Long", "Ptr", "Short", "String"
					' NOP
				Default
					If sb.Length() > 0 Then
						sb.Append(" " )
					End If
					sb.Append(token)
			End Select
		Next
		Return sb.ToString()
	End Function

	Method SetOptions(options:String = "" )
		
		options = options.Trim().toUpper()
		
		If options.contains("STDOUT" ) Then optStdOut = True
		If options.contains("STDIN" ) Then optStdIn = True
		
	EndMethod
	
	Method SetTraceWords:Int( words:String )
		If Not words Then Return False
		
		Local ar:String[] = words.split(" " )
		If Not ar Then Notify "Error: Could not set traceable keywords.", True; Return False
		
		For Local i:Int = 0 Until ar.length
			
			Local tmpKey:String = ar[i].Trim().tolower()
			If Not tmpKey Then Continue
			
			For Local w:TWord = EachIn _wordList
				If w.key = tmpKey Then
					w.trace = True; Exit
				EndIf
			Next
		Next
		
		Return True
	EndMethod

	Method _parse:String( txt:String Var, start:Int = 0 )
			
		If Not txt Then Return txt
		
		Local sb:TStringBuilder = New TStringBuilder
		Local startPos:Int = - 1, lineCount:Int = start
		Local IsString:Int, isCom:Int, isRem:Int, isNewline:Int, isEnd:Int
		Local isParam:Int
		Local isIdentifier:Int
		Local token:String, name:String, char:Int, analyze:Int
		Local eol:Int = txt.length - 1
		Local lastProcessed:Int = 0  ' Track what we've already appended
		
		For Local i:Int = 0 Until txt.length
			
			Select txt[i]
				
				Case 39 'Comment
					
					If IsString Or isRem Then Continue			
					isCom = 1
					
				Case 34	'String
					
					If isCom Or isRem Then Continue
					IsString = Not IsString
					
					If IsString And startPos > - 1 Then
						analyze = 1
					Else
						startPos = - 1
					EndIf

				Case 9, 32	'Tab, Space 
					
					If startPos > - 1 And Not isIdentifier Then analyze = 1
					
				Case 10		'Newline
						
					IsString = 0
					isCom = 0
					
					If startPos > - 1 Then
						isNewline = 1
						analyze = 1
					Else
						isIdentifier = 0
						lineCount:+1
					EndIf
			
				Case 13, 59		' Return, Semicolon ';'
				
					If startPos > - 1 Then analyze = 1
					
				' Operator detection: = + - * / < > & | ^ ~ ! % :
				Case 61, 43, 45, 42, 47, 60, 62, 38, 124, 94, 126, 33, 37, 58
					If IsString Or isCom Or isRem Then Continue

					' Determine operator and length. Detect colon-based compounds even if they have spaces in between.
					Local isCompound:Int = 0
					Local opLen:Int = 1
					Local op:String = Chr( txt[i] )
					Local specialColonOp:Int = 0

					If i < eol Then
						Select txt[i]
							Case 60 ' '<'
								If txt[i+1] = 61 Then isCompound = 1 ; op = "<=" ; opLen = 2
								If txt[i+1] = 62 Then isCompound = 1 ; op = "<>" ; opLen = 2
							Case 62 ' '>'
								If txt[i+1] = 61 Then isCompound = 1 ; op = ">=" ; opLen = 2
							Case 58 ' ':'
								' Look ahead skipping spaces/tabs to find + - * /
								Local j:Int = i + 1
								While j <= eol And ( txt[j] = 32 Or txt[j] = 9 )
									j:+1
								Wend
								If j <= eol Then
									Select txt[j]
										Case 43, 45, 42, 47 ' + - * /
											isCompound = 1
											op = ":" + Chr( txt[j] )
											opLen = j - i + 1 ' includes any intervening spaces
											specialColonOp = 1
									EndSelect
								EndIf
						EndSelect
					EndIf

					' If there is an unfinished token, finalise it now (same logic used in the analyze block)
					If startPos > - 1 Then
						name = txt[startPos..i]
						token = name.tolower()

						' Append any non-token text between lastProcessed and startPos
						If lastProcessed < startPos Then
							sb.Append(txt[lastProcessed..startPos])
						EndIf

						If Not isIdentifier Then
							If token = "end"
								isEnd = 1
							ElseIf token = "endrem" Or token = "end rem" Then
								isRem = 0
							ElseIf token = "rem"
								isRem = 1
							EndIf

							name = GetWord(token)

							' Ensure keywords have trailing space if needed
							If _isKeywordNeedingSpace(name) And i < txt.length Then
								Local nextChar:Int = txt[i]
								If nextChar <> 32 And nextChar <> 9 And nextChar <> 10 And nextChar <> 13 And nextChar <> 40 Then
									sb.Append(" ")
								EndIf
							EndIf

							If _isMatch Then
								If optAutoCap And name <> token Then
									sb.Append(name)
								Else
									sb.Append(txt[startPos..i])
								EndIf

								isIdentifier = _isTraceable
								If isIdentifier And isEnd Then
									isEnd = 0
									isIdentifier = 0
								EndIf
							Else
								sb.Append(txt[startPos..i])
							EndIf
						Else
							_parseTraceable(name, lineCount)
							isIdentifier = 0
							sb.Append(txt[startPos..i])
						EndIf

						lastProcessed = i
						startPos = - 1
					EndIf

					' Append the intervening text up to the operator, trimming trailing spaces
					If lastProcessed < i Then
						Local preOpText:String = txt[lastProcessed..i]
						Local endPos:Int = preOpText.length - 1
						While endPos >= 0 And ( preOpText[endPos] = 32 Or preOpText[endPos] = 9 )
							endPos:-1
						Wend
						If endPos >= 0 Then sb.Append(preOpText[..endPos+1])
					EndIf

					' Emit operator according to rules:
					'  - single-char operators (except ':') -> space both sides
					'  - ':' itself -> attach (no spaces)
					'  - two-char ops -> space both sides, except colon-prefixed two-char (':+' etc) -> no spaces (attach)
					'  - fallback -> append raw
					If specialColonOp Then
						' attach colon compound with no spaces (consumes any spaces that were between)
						sb.Append(op)
					ElseIf opLen = 1 Then
						If op = ":" Then
							sb.Append(":")
						ElseIf op = "-" Or op = "+" Then
							' Detect unary plus/minus: if previous non-space char is an operator or '('
							Local prev:Int = sb.Length() - 1
							While prev >= 0 And ( sb[prev] = 32 Or sb[prev] = 9 )
								prev:-1
							Wend
							Local prevChar:Int = - 1
							If prev >= 0 Then prevChar = sb[prev]
							
							If prev < 0 Or prevChar = 40 Or prevChar = 61 Or prevChar = 43 Or prevChar = 45 Or prevChar = 42 Or prevChar = 47 Or prevChar = 60 Or prevChar = 62 Then
								' Unary: no leading space
								sb.Append(op + " ")
							Else
								' Binary: space both sides
								sb.Append(" " + op + " ")
							EndIf
						Else
							sb.Append(" " + op + " ")
						EndIf
					ElseIf opLen = 2 Then
						sb.Append(" " + op + " ")
					ElseIf opLen = 4 Then
						sb.Append(op + " ")
					Else
						sb.Append(op)
					EndIf

					' Advance index to skip the operator and any intervening chars we consumed
					i:+(opLen - 1)

					' Skip spaces and tabs immediately after the operator (we already consumed any that were part of a colon compound)
					While i + 1 < txt.length And ( txt[i+1] = 32 Or txt[i+1] = 9 )
						i:+1
					Wend

					lastProcessed = i + 1
					Continue
						
				Default
					
					If i = eol And startPos > - 1 Then analyze = 1; i:+1
			
					isNewline = 0
					If IsString Or isCom Then Continue
					If startPos = - 1 Then startPos = i
					
			EndSelect
			
			'Words are analyzed at this point
			'--------------------------------
			If analyze Then
				
				analyze = 0
				name = txt[startPos..i]
				token = name.tolower()
				
				' Append everything from lastProcessed up to startPos
				If lastProcessed < startPos Then
					sb.Append(txt[lastProcessed..startPos])
				EndIf
				
				If Not isIdentifier Then
					
					If token = "end"
						isEnd = 1	'; Continue
					ElseIf token = "endrem" Or token = "end rem" Then
						isRem = 0
					ElseIf token = "rem"
						isRem = 1
					EndIf
					
					name = GetWord(token)

					' Ensure keywords have trailing space if needed
					If _isKeywordNeedingSpace(name) And i < txt.length Then
						Local nextChar:Int = txt[i]
						If nextChar <> 32 And nextChar <> 9 And nextChar <> 10 And nextChar <> 13 And nextChar <> 40 Then
							sb.Append(" ")
						EndIf
					EndIf

					If _isMatch Then
						If optAutoCap And name <> token Then
							' Append the corrected case version
							sb.Append(name)
						Else
							' Append original token
							sb.Append(txt[startPos..i])
						EndIf
						
						isIdentifier = _isTraceable
						
						If isIdentifier And isEnd Then
							isEnd = 0
							isIdentifier = 0
						EndIf
						
					Else
						' No match - append original token
						sb.Append(txt[startPos..i])
					EndIf
					
				Else
					'Name of the Method, Function etc.
					_parseTraceable(name, lineCount)
					isIdentifier = 0
					' Append the identifier as-is
					sb.Append(txt[startPos..i])
				EndIf
				
				' Update lastProcessed to current position
				lastProcessed = i
				
				If isNewline Then
					isNewline = 0
					lineCount:+1
				EndIf
				
				startPos = - 1
			EndIf
		Next
		
		' Append any remaining text after the last processed position
		If lastProcessed < txt.length Then
			sb.Append(txt[lastProcessed..])
		EndIf
		
		Local result:String = sb.ToString()

		' Apply bracket spacing fix
		
		result = _fixBracketSpacing(result)
		result = _fixArrayBrackets(result)
		result = _fixGeneralCommas(result)
	
		If optStdOut Then
			mWrite.Lock()
			WriteStdout( result )
			mWrite.UnLock()
			Return ""
		Else	
			Return result
		EndIf

	EndMethod

	Method _isKeywordNeedingSpace:Int( word:String )
		word = word.ToLower()
		Select word
			Case "if", "while", "for", "select", "case", "elseif", "until", "repeat"
				Return True
			Default
				Return False
		EndSelect
	EndMethod

	Function _fixGeneralCommas:String( txt:String )
		Local sb:TStringBuilder = New TStringBuilder
		Local i:Int = 0
		Local inString:Int = False
		Local inComment:Int = False
		Local inParens:Int = 0
		Local inBrackets:Int = 0
		
		While i < txt.length
			Local c:Int = txt[i]
			
			' Handle comments - preserve everything as-is
			If c = 39 And Not inString Then ' single quote
				inComment = True
				sb.Append(Chr( c ))
				i:+1
				' Copy rest of comment line as-is
				While i < txt.length And txt[i] <> 10 And txt[i] <> 13
					sb.Append(Chr( txt[i] ))
					i:+1
				Wend
				inComment = False
				Continue
			EndIf
			
			If c = 10 Or c = 13 Then ' newline/return
				inComment = False
			EndIf
			
			If inComment Then
				sb.Append(Chr( c ))
				i:+1
				Continue
			EndIf
			
			If c = 34 Then
				inString = Not inString
				sb.Append(Chr( c ))
				i:+1
				Continue
			EndIf
			
			If Not inString Then
				If c = 40 Then ' (
					inParens:+1
				ElseIf c = 41 Then ' )
					inParens:-1
				ElseIf c = 91 Then ' [
					inBrackets:+1
				ElseIf c = 93 Then ' ]
					inBrackets:-1
				ElseIf c = 44 And inParens = 0 And inBrackets = 0 Then ' comma outside parens/brackets
					' Remove trailing spaces before comma
					While sb.Length() > 0 And ( sb.ToString()[sb.Length()-1] = 32 Or sb.ToString()[sb.Length()-1] = 9 )
						sb.SetLength(sb.Length() - 1)
					Wend
					sb.Append(", ")
					i:+1
					' Skip spaces after comma
					While i < txt.length And ( txt[i] = 32 Or txt[i] = 9 )
						i:+1
					Wend
					Continue
				EndIf
			EndIf
			
			sb.Append(Chr( c ))
			i:+1
		Wend
		
		Return sb.ToString()
	End Function

	Function _fixArrayBrackets:String( txt:String )
		Local outSb:TStringBuilder = New TStringBuilder
		Local i:Int = 0
		Local inString:Int = False
		Local inComment:Int = False
		
		While i < txt.length
			Local c:Int = txt[i]
			
			' Handle comments - preserve everything after ' until newline
			If c = 39 And Not inString Then ' single quote
				inComment = True
				outSb.Append(Chr( c ))
				i:+1
				' Copy rest of comment line as-is
				While i < txt.length And txt[i] <> 10 And txt[i] <> 13
					outSb.Append(Chr( txt[i] ))
					i:+1
				Wend
				inComment = False
				Continue
			EndIf
			
			If c = 34 Then   ' toggle string
				inString = Not inString
				outSb.Append(Chr( c ))
				i:+1
				Continue
			EndIf
			
			If c = 10 Or c = 13 Then ' newline/return
				inComment = False
				outSb.Append(Chr( c ))
				i:+1
				Continue
			EndIf
			
			If inComment Then
				outSb.Append(Chr( c ))
				i:+1
				Continue
			EndIf
			
			If Not inString And c = 91 Then   ' opening [
				outSb.Append("[")
				i:+1
				' strip spaces immediately after [
				While i < txt.length And ( txt[i] = 32 Or txt[i] = 9 )
					i:+1
				Wend
				' now copy until ], handling commas properly
				While i < txt.length And txt[i] <> 93
					If txt[i] = 32 Or txt[i] = 9 Then
						' skip spaces unless we're preserving string content
						i:+1
						Continue
					ElseIf txt[i] = 44   ' comma
						outSb.Append(", ")
						i:+1  ' FIXED: was i :+ 2
						' strip any additional spaces after comma
						While i < txt.length And ( txt[i] = 32 Or txt[i] = 9 )
							i:+1
						Wend
					Else
						outSb.Append(Chr( txt[i] ))
						i:+1
					EndIf
				Wend
				If i < txt.length And txt[i] = 93 Then
					outSb.Append("]")
					i:+1
				EndIf
				Continue
			EndIf
			
			outSb.Append(Chr( c ))
			i:+1
		Wend
		
		Return outSb.ToString()
	End Function


	' Normalise commas inside a parenthesis argument string.
	' Ensures no space before comma, exactly one space after comma, and preserves strings.
	Method _fixArgumentCommas:String( s:String )
		Local outSb:TStringBuilder = New TStringBuilder
		Local i:Int = 0
		Local Instr:Int = 0

		While i < s.length
			Local ch:Int = s[i]

			If ch = 34 Then ' "
				outSb.Append(Chr( ch ))
				Instr = Not Instr
				i:+1
				Continue
			EndIf

			If ch = 44 And Not Instr Then ' comma outside string
				' Remove trailing spaces/tabs before comma
				While outSb.Length() > 0 And ( outSb.ToString()[outSb.Length()-1] = 32 Or outSb.ToString()[outSb.Length()-1] = 9 )
					outSb.SetLength(outSb.Length() - 1)
				Wend

				' Append comma
				outSb.Append(", ")

				' Skip spaces/tabs after the comma in source
				i:+1
				While i < s.length And ( s[i] = 32 Or s[i] = 9 )
					i:+1
				Wend

				' Add exactly one space if not end of arguments
				If i < s.length Then outSb.Append(" ")
				Continue
			EndIf

			outSb.Append(Chr( ch ))
			i:+1
		Wend

		Return outSb.ToString()
	EndMethod


	' Fix bracket spacing for commands only. This consumes the original parentheses and emits a
	' single formatted "( ... )" or "()" as appropriate. Preserves nested parens and strings.
	Method _fixBracketSpacing:String( txt:String )
		Local outSb:TStringBuilder = New TStringBuilder
		Local i:Int = 0
		Local inComment:Int = False

		While i < txt.length
			Local ch:Int = txt[i]

			' Handle comments - preserve everything as-is including all spacing
			If ch = 39 And Not inComment Then ' single quote
				inComment = True
				outSb.Append(Chr( ch ))
				i:+1
				' Copy rest of comment line as-is, preserving all spacing
				While i < txt.length And txt[i] <> 10 And txt[i] <> 13
					outSb.Append(Chr( txt[i] ))
					i:+1
				Wend
				inComment = False
				Continue
			EndIf
			
			If ch = 10 Or ch = 13 Then ' newline/return
				inComment = False
				outSb.Append(Chr( ch ))
				i:+1
				Continue
			EndIf
			
			If inComment Then
				outSb.Append(Chr( ch ))
				i:+1
				Continue
			EndIf

			If ch = 40 Then ' '('
				' Find preceding word (skip whitespace backwards first)
				Local k:Int = i - 1
				While k >= 0 And ( txt[k] = 32 Or txt[k] = 9 )
					k:-1
				Wend

				Local wordEnd:Int = k
				If wordEnd >= 0 Then
					' Scan backward to find start of word (letters, digits, underscore)
					Local k2:Int = wordEnd
					While k2 >= 0
						Local c:Int = txt[k2]
						If ( c >= 65 And c <= 90 ) Or ( c >= 97 And c <= 122 ) Or ( c >= 48 And c <= 57 ) Or c = 95 Then
							k2:-1
						Else
							Exit
						EndIf
					Wend
					Local wordStart:Int = k2 + 1

					If wordStart <= wordEnd Then
						' Build the word string
						Local wSb:TStringBuilder = New TStringBuilder
						For Local p:Int = wordStart To wordEnd
							wSb.Append(Chr( txt[p] ))
						Next
						Local word:String = wSb.ToString().ToLower()

						' If this is a command word, process the parentheses content
						If _isCommandWord(word) Then
							' Collect everything inside the matching parentheses,
							' respecting nested parens and string literals.
							Local j:Int = i + 1
							Local depth:Int = 0
							Local Instr:Int = 0
							Local innerSb:TStringBuilder = New TStringBuilder

							While j < txt.length
								Local cj:Int = txt[j]

								If cj = 34 Then ' quote
									innerSb.Append(Chr( cj ))
									Instr = Not Instr
									j:+1
									Continue
								EndIf

								If Not Instr Then
									If cj = 40 Then ' '(' nested
										depth:+1
										innerSb.Append(Chr( cj ))
										j:+1
										Continue
									EndIf

									If cj = 41 Then ' closing paren
										If depth = 0 Then
											' Found matching closing paren for the command
											Exit
										Else
											depth:-1
											innerSb.Append(Chr( cj ))
											j:+1
											Continue
										EndIf
									EndIf
								EndIf

								innerSb.Append(Chr( cj ))
								j:+1
							Wend

							' j is now index of the closing ')', or txt.length if not found
							Local inner:String = innerSb.ToString()

							' Trim leading/trailing spaces and tabs from inner
							Local sStart:Int = 0
							While sStart < inner.length And ( inner[sStart] = 32 Or inner[sStart] = 9 )
								sStart:+1
							Wend
							Local sEnd:Int = inner.length - 1
							While sEnd >= sStart And ( inner[sEnd] = 32 Or inner[sEnd] = 9 )
								sEnd:-1
							Wend

							Local core:String
							If sEnd >= sStart Then
								core = inner[sStart..sEnd+1]
							Else
								core = ""
							EndIf

							If core = "" Then
								' Empty argument list: keep exactly "()"
								outSb.Append("()")
							Else
								' Normalise commas and add surrounding spaces
								core = _fixArgumentCommas(core)
								outSb.Append("( " + core + " )")
							EndIf

							' Advance i to the closing paren (if found); if not found, jump to end
							If j < txt.length Then
								i = j + 1
								Continue
							Else
								' Unmatched paren, append nothing more and break
								i = j
								Continue
							EndIf
						EndIf ' is command word
					EndIf ' wordStart <= wordEnd
				EndIf ' wordEnd >= 0

				' Not a command word or no valid word before '('; just append '(' normally
				outSb.Append("(")
				i:+1
				Continue
			EndIf ' '('

			' Default behaviour: copy the char
			outSb.Append(Chr( ch ))
			i:+1
		Wend

		Local formatted:String = outSb.ToString()
		' Final pass: also normalise commas outside of parens
		formatted = _fixArgumentCommas(formatted)
		Return formatted
	EndMethod

	Method _isCommandWord:Int( word:String )
		' Check if this word is a command (not a declaration keyword)
		word = word.tolower()
		If word = "function" Or word = "method" Or word = "type" Then Return False
		
		' Check if it's in our keywords list (commands)
		Local testWord:TWord
		For testWord = EachIn _wordList
			If testWord.key = word And Not testWord.trace Then
				Return True
			EndIf
		Next
		
		Return False
	EndMethod

	Method _parseTraceable(token:String, line:Int = - 1)
		
		'DebugLog "ParseTraceable"
		
		Local traceType:String = _traceType
		
		If Not token Or Not traceType Or line = - 1 Then Return
		
		Local key:String
		
		'Is a Function or a Method
		If token.contains("(" )
			token = token.Replace( " ","" ).Replace( "~t" , "" ).Replace( ")"  , "" ) 
			Local ar1:String[] = token.split("(" )
			Local ar2:String[] = ar1[0].split(":" )
			Local par:String[] = ar1[1].split(", " )
			
			key = traceType.tolower() + "_" + ar2[0]
			token = ":".join(ar2) + "(" + ", ".join(par) + ")"

		ElseIf token.contains(":" )
			token = token.Replace( " ", "" ).Replace( "~t", "" )
			Local ar:String[] = token.split(":" )		
		EndIf
		
		'DebugLog token + " | line = " + line + " | key = " + key + " | level = " + GetFoldLevel(line) + " | parent level = " + GetFoldParent(line)
		
		Local w:TWord = New TWord
		w.name = token
		w.key = key
		w.line = line
		'w.parentLine = GetFoldParent(line)
		
		'Local m:TModified = TModified.CreateWord(MODIFIED_TREE_ADD_TRACEABLE, w)
		'TExplorer.SetEvent(m)
	EndMethod
	
EndType

Type TWord
	
	Field style:Int
	Field name:String
	Field key:String
	Field trace:Int
	Field line:Int
	Field traceKey:String	
	
	Method compare:Int( o:Object )
		Local w:TWord = TWord(o)
		Return w.name.compare(Self.name)
	EndMethod
	
EndType

Type TBMFStream Extends TTextStream
	
	Global str:TBMFStream
	Global text:TStringBuilder = New TStringBuilder
	
	Function Create:TBMFStream()
		
		Local stream:TStream = New TCStandardIO
		
		str = New TBMFStream
		str._encoding = ETextStreamFormat.UTF8
		str.SetStream(stream)
		
		StandardIOStream = str
		
		Return str
	End Function

EndType

Function CacheAndLoadText$(url:Object)
	Local tmpResult$
	Local tmpBytes:Byte[] = LoadByteArray( url )
	url = CreateRamStream( tmpBytes,tmpBytes.Length,  True , False )
	tmpResult = LoadText(  url  )
	TRamStream(url).Close()
	Return tmpResult
EndFunction
