# cl-ascii-table
---
Common Lisp library to present tabular data in ascii-art table.

### Installation
1. Clone into a directory visible to quicklisp  
```
git clone https://github.com/telephil/cl-ascii-table
```
2. Load the library  
```
(ql:quickload "cl-ascii-table")
```

### API
---
> `function`  
> **make-table** _columns &key header => ascii-table_

Create a new table
* `columns` a list of strings
* `header` title to display above the table if not `nil`
  
  
---
> `method`  
> **add-row** _(table ascii-table) columns_

Add a row of values to `table`
* `columns` a list of values. The list length should be equal to the length of the columns list passed in `make-table`

---
> `method`  
> **add-separator** _(table ascii-table)_

Add a separating line in the `table`

---
> `method`  
> **display-table** _(table ascii-table) &optional out_

Display the `table` to `out`.
* `out` an open stream. Defaults to _\*standard-output\*_

### Sample
---
```lisp
(let ((table (ascii-table:make-table '("Id" "Name" "Amount"))))
  (ascii-table:add-row table '(1 "Bob" 150))
  (ascii-table:add-row table '(2 "Joe" 200))
  (ascii-table:add-separator table)
  (ascii-table:add-row table '("" "Total" 350))
  (ascii-table:display table)))

; => outputs
.---------------------.  
|        Infos        |
+----+-------+--------+  
| ID | Name  | Amount |  
+----+-------+--------+  
|  1 | Bob   |    150 |  
|  2 | Joe   |    200 |  
+----+-------+--------+  
|    | TOTAL |    350 |  
+----+-------+--------+ 
```

### License
Copyright (C) 2014, Philippe MECHAI
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer. 
Redistributions in binary form must reproduce the above copyright notice,
this list of conditions and the following disclaimer in the documentation
and/or other materials provided with the distribution. 
Neither the name of the author nor the names of its contributors may be
used to endorse or promote products derived from this software without
specific prior written permission. 

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR CONTRIBUTORS BE
LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.

