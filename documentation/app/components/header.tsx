
import {
  NavigationMenu
  , NavigationMenuItem
  , NavigationMenuLink
  , NavigationMenuList
} from "~/components/ui/navigation-menu";
import GitHubStars from "~/components/GitHubStars";
import { ModeToggle } from "@/components/ui/mode-toggle";
import hackage from "/assets/hackage.svg";
import { Link } from "react-router";
import logo from "/assets/logo.svg";


export function Header() {
  return (
    <header className="w-full flex bg-background  align-middle sticky top-0 h-14 border-b pr-1 pl-1">
      <NavigationMenu className="w-full " viewport={false}>
        <NavigationMenuList className="flex justify-between">
          <NavigationMenuItem>
            <NavigationMenuLink asChild>
              <Link to="/"><img alt="logo" className="size-5" src={logo} /></Link>
            </NavigationMenuLink>
          </NavigationMenuItem>
          <NavigationMenuItem>
            <NavigationMenuLink asChild>
              <Link to="/contribution">/contribution</Link>
            </NavigationMenuLink>
          </NavigationMenuItem>
          <NavigationMenuItem>
            <NavigationMenuLink asChild>
              <Link to="/testing">/testing</Link>
            </NavigationMenuLink>
          </NavigationMenuItem>
          <NavigationMenuItem>
            <NavigationMenuLink asChild>
              <Link to="/usage/">/usage</Link>
            </NavigationMenuLink>
          </NavigationMenuItem>
          {/* <li><Link to="/performance">/performance</Link></li> */}
          {/* <li><a href="/protocol/server/">/protocol/server</a></li> */}
          {/* <li><a href="/protocol/client/">/protocol/client</a></li> */}
        </NavigationMenuList>
        <NavigationMenuList>
          <NavigationMenuItem>
            <NavigationMenuLink asChild>
              <Link to="https://hackage.haskell.org/package/ClickHaskell" className="flex-row items-center h-9">
                <img alt="hckg" width="24" height="24" src={hackage} />
              </Link>
            </NavigationMenuLink>
          </NavigationMenuItem>
          <NavigationMenuItem >
            <NavigationMenuLink asChild>
              <Link to="https://git.clickhaskell.dev" className="flex-row items-center h-9">
                <img alt="git" width="24" height="24" src="/assets/git.svg" />
                <GitHubStars />
              </Link>
            </NavigationMenuLink>
          </NavigationMenuItem>
        </NavigationMenuList>
      </NavigationMenu>
    </header>
  )
}
