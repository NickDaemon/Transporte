module PPON where

import Documento

data PPON
  = TextoPP String
  | IntPP Int
  | ObjetoPP [(String, PPON)]
  deriving (Eq, Show)

-- Mini ejemplos de prueba
pericles = ObjetoPP [("nombre", TextoPP "Pericles"), ("edad", IntPP 30)]
merlina  = ObjetoPP [("nombre", TextoPP "Merlina"), ("edad", IntPP 24)]
addams   = ObjetoPP [("0", pericles), ("1", merlina)]
familias = ObjetoPP [("Addams", addams)]
prueba   = TextoPP "Soy un ppon Atomico uwu"

pponAtomico :: PPON -> Bool
pponAtomico ppon = case ppon of
  TextoPP _ -> True
  IntPP _   -> True
  _         -> False

pponObjetoSimple :: PPON -> Bool
pponObjetoSimple ppon = case ppon of
  ObjetoPP lista -> foldr fAux True lista
  _              -> False
  where
    fAux (_,ppon) acc = pponAtomico ppon && acc

intercalar :: Doc -> [Doc] -> Doc
intercalar s [] = vacio
intercalar s d1 = foldr1 (\t acc -> t <+> s <+> acc) d1

entreLlaves :: [Doc] -> Doc
entreLlaves [] = texto "{ }"
entreLlaves ds =
  texto "{"
    <+> indentar
      2
      ( linea
          <+> intercalar (texto "," <+> linea) ds
      )
    <+> linea
    <+> texto "}"

aplanar :: Doc -> Doc
aplanar = foldDoc vacio fTexto fLinea 
  where
    fTexto t acc = texto t <+> acc
    fLinea s acc = texto " " <+> acc

pponADoc :: PPON -> Doc
pponADoc ppon = case ppon of
  TextoPP s      -> texto (show s)
  IntPP i        -> texto (show i)
  ObjetoPP lista -> if pponObjetoSimple (ObjetoPP lista) then f lista else g lista
  where
    f lista      = aplanar (entreLlaves (map h lista))
    g lista      = entreLlaves (map h lista)
    h (s, ppon)  = texto (show s) <+> texto ": " <+> pponADoc ppon

-- pponADoc TextoPP = z
-- pponADoc IntPP   = z
-- pponADoc (ObjetoPP (x:xs)) = f x ... g xs ... 
    
    