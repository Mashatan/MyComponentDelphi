{
   Author:   Ali Mashatan
   E-mail:  ali.mashatan@gmail.com
   website: https://github.com/Mashatan
   Date:     2008-02-28
   License:  GNU Public License ( GPL3 ) [http://www.gnu.org/licenses/gpl-3.0.txt]
}

unit MSPopupMenu;

interface
uses Menus;
type
  TMSPopupMenu=class (TPopupMenu)
    private
      fNode:string;
    public
      procedure Popup(X, Y: Integer; Node:string);
      property Node:string read fNode;
  end;
implementation

{ TMSPopupMenu }

procedure TMSPopupMenu.Popup(X, Y: Integer; Node: string);
begin
   fNode := Node;
   inherited Popup(x,y);
end;

end.
