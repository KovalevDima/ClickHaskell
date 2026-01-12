
import {
  NavigationMenu
  , NavigationMenuItem
  , NavigationMenuLink
  , NavigationMenuList
} from "~/components/ui/navigation-menu";
import { ModeToggle } from "@/components/ui/mode-toggle";
import { Link } from "react-router";
import { SidebarTrigger } from "./ui/sidebar";


type HeaderItems = {
  items: {
    component : React.ReactNode,
    link: string,
  }[],
}

export function Header({items } : HeaderItems) {
  return (
    <header className="w-full flex bg-background  align-middle sticky top-0 h-14 border-b pr-1 pl-1">
      <NavigationMenu className="w-full " viewport={false}>
        <NavigationMenuList className="flex justify-between">
          <NavigationMenuItem>
            <SidebarTrigger className="size-8"/>
          </NavigationMenuItem>
        </NavigationMenuList>
        <NavigationMenuList>
          {items.map((item) => (
            <NavigationMenuItem>
              <NavigationMenuLink asChild >
                <Link to={item.link} className="flex-row items-center h-9">
                  {item.component}
                </Link>
              </NavigationMenuLink>
            </NavigationMenuItem>
          ))}
        </NavigationMenuList>
      </NavigationMenu>
    </header>
  )
}
