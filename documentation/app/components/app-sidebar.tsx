import * as React from "react"
import {
  SquareTerminal,
  BugOff,
  GitPullRequestCreate,
  PackagePlus,
  LucideIcon,
  ChevronRight,
  Microscope,
  Binary,
} from "lucide-react"

import {
  Sidebar,
  SidebarContent,
  SidebarFooter,
  SidebarGroup,
  SidebarGroupLabel,
  SidebarHeader,
  SidebarMenu,
  SidebarMenuButton,
  SidebarMenuItem,
  SidebarMenuSub,
  SidebarMenuSubButton,
  SidebarMenuSubItem,
} from "@/components/ui/sidebar"
import { ModeToggle } from "./ui/mode-toggle"
import { Link } from "react-router"
import logo from "/assets/logo.svg";
import { Collapsible, CollapsibleContent, CollapsibleTrigger } from "./ui/collapsible"

const data = {
  navMain: [
    {
      title: "Build",
      url: "/usage",
      icon: PackagePlus,
      isActive: true,
      items: [
        {
          title: "All-in-one",
          icon: PackagePlus,
          url: "/usage",
        },
      ]
    },

    {
      title: "Learn",
      url: "/contribution",
      icon: Microscope,
      isActive: true,
      items: [
        {
          title: "Contribution",
          icon: GitPullRequestCreate,
          url: "/contribution",
        },
        {
          title: "About QA",
          icon: BugOff,
          url: "/testing",
        },
      ]
    },

    {
      title: "Protocol",
      url: "/protocol",
      icon: Binary,
      isActive: true,
      items: [
        {
          title: "Client",
          icon: GitPullRequestCreate,
          url: "/protocol/client",
        },
        {
          title: "Server",
          icon: BugOff,
          url: "/protocol/server",
        },
        {
          title: "Common",
          icon: BugOff,
          url: "/protocol/common",
        },
      ]
    },
  ]
};
  
export function AppSidebar({ ...props }: React.ComponentProps<typeof Sidebar>) {
  return (
    <Sidebar
      collapsible="icon"
      {...props}
    >
      <SidebarHeader className="h-(--header-height) bg-background">
        <SidebarMenu>
          <SidebarMenuItem>
            <SidebarMenuButton size="lg" asChild>
              <Link to="/">
                <div className="flex aspect-square size-8 items-center justify-center rounded-lg">
                  <img alt="logo" className="size-5" src={logo} />
                </div>
                <div className="grid flex-1 text-left text-sm leading-tight">
                  <span className="truncate font-medium">ClickHaskell</span>
                  <span className="truncate text-xs">overview</span>
                </div>
              </Link>
            </SidebarMenuButton>
          </SidebarMenuItem>
        </SidebarMenu>
      </SidebarHeader>
      <SidebarContent className="bg-background">
        <NavMain items={data.navMain} />
      </SidebarContent>
      <SidebarFooter className="bg-background">
        <ModeToggle />
      </SidebarFooter>
    </Sidebar>
  )
}

export function NavMain({
  items,
}: {
  items: {
    title: string
    url: string
    icon?: LucideIcon
    isActive?: boolean
    items?: {
      title: string
      url: string
    }[]
  }[]
}) {
  return (
    <SidebarGroup>
      {/* <SidebarGroupLabel>Platform</SidebarGroupLabel> */}
      <SidebarMenu>
        {items.map((item) => (
          <Collapsible
            key={item.title}
            asChild
            defaultOpen={item.isActive}
            className="group/collapsible"
          >
            <SidebarMenuItem>
              <CollapsibleTrigger asChild>
                <SidebarMenuButton tooltip={item.title}>
                  {item.icon && <item.icon />}
                  <span>{item.title}</span>
                  <ChevronRight className="ml-auto transition-transform duration-200 group-data-[state=open]/collapsible:rotate-90" />
                </SidebarMenuButton>
              </CollapsibleTrigger>
              <CollapsibleContent>
                <SidebarMenuSub>
                  {item.items?.map((subItem) => (
                    <SidebarMenuSubItem key={subItem.title}>
                      <SidebarMenuSubButton asChild>
                        <a href={subItem.url}>
                          <span>{subItem.title}</span>
                        </a>
                      </SidebarMenuSubButton>
                    </SidebarMenuSubItem>
                  ))}
                </SidebarMenuSub>
              </CollapsibleContent>
            </SidebarMenuItem>
          </Collapsible>
        ))}
      </SidebarMenu>
    </SidebarGroup>
  )
}

