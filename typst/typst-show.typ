#show: doc => cv(
$if(title)$
  title: "$title$",
$endif$
$if(by-author)$
  author: ($for(by-author)$"$it.name.literal$",$endfor$),
$endif$
$if(lang)$
  lang: "$lang$",
$endif$
  doc,
)
