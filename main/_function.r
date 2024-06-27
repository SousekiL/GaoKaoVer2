## 分专业最低位次
# In summary, this function filters and groups a dataframe based on certain conditions, calculates the maximum value of a column for each group, and renames a column.
getLineByMajor <- function(data, text, year) {
  data_s <-  data %>%
    dplyr::filter(!is.infinite(位次),
                  !is.na(位次)) %>%
    dplyr::filter(grepl(text, .$专业)) %>%
    dplyr::group_by(`院校`) %>%
    dplyr::summarise(rank = max(`位次`, na.rm = TRUE)) %>%
    ungroup() %>%
    dplyr::rename_with(~glue('rank{year}'), 'rank')
  return(data_s)
}


# This code defines two vectors: .noun and .major.
#  # nolint
# The .noun vector contains a list of keywords representing different fields of study or professions. # nolint
# Each element in the vector is a regular expression pattern that matches specific keywords. # nolint
# The fields/professions represented in the vector include news, communication, advertising, publishing, translation, foreign languages, law, Chinese language, philosophy, finance, business management, political science, psychology, public administration, social work, anthropology, sociology, mathematics, electrical engineering, telecommunications, mechanical engineering, computer science, artificial intelligence, software engineering, civil engineering, architecture, biology, materials science, chemistry, environmental science, medicine, agriculture, forestry, animal science, and aquaculture. # nolint: line_length_linter.
# 
# The .major vector contains a list of specific majors or areas of study corresponding to the fields/professions represented in the .noun vector. # nolint
# Each element in the vector is a string representing a specific major or area of study. # nolint: line_length_linter.
# The majors/areas of study represented in the vector include journalism and communication, foreign languages and literature, law, Chinese language, philosophy, finance, business management, political science education, psychology, public administration, sociology, mathematics, electrical engineering, information and communication engineering, mechanical engineering, computer science and technology, software engineering, civil engineering, architecture, biology, materials science and engineering, chemistry, environmental science and engineering, medicine, and agriculture.
.noun <- c('新闻|传播|广告|出版', '翻译|外语|外国语|.*?语$', '法学|法律', '中文|汉语言', '哲学', '金融', '工商管理',
           '思想政治', '心理', '公共管理|行政管理|社会保障', '社会学|社会工作|人类学|民族学|民俗学',
           '数学', '电气', '通信', '机械',
           '计算机|人工智能', '软件',  '土木', '建筑|城乡规划',
           '生物', '材料', '化学', '环境科学|环境工程', 
           '医学|口腔|临床|药学|公共卫生|护理', '农|林学|林业|草|动物|水产')
.major <- c('新闻传播学', '外国语言文学', '法学', '中文', '哲学', '金融类', '工商管理类',
            '思想政治教育', '心理学', '公共管理类', '社会学类',
            '数学', '电气工程', '信息与通信工程', '机械工程',
            '计算机科学与技术',  '软件工程',  '土木工程', '建筑学',
            '生物学', '材料科学与工程', '化学', '环境科学与工程', 
            '医学', '农学')
