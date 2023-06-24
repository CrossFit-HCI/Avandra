import { configureStore, createSlice } from "@reduxjs/toolkit";
import { Provider, TypedUseSelectorHook, useDispatch, useSelector } from "react-redux";
import React, { Children, JSXElementConstructor, ReactElement, ReactNode, createContext } from "react";
import { Button, GestureResponderEvent } from "react-native";

import Maybe, { just, nothing } from "./Maybe";
import { BinTree, empty, insert, lookup, mkKeyString } from "./Map";

interface NavOpened {
    status: 'opened';
}
  
interface NavClosed {
    status: 'closed';
}

type NavStatus = NavOpened | NavClosed;

export const navOpened: NavStatus = {status: 'opened'}
export const navClosed: NavStatus = {status: 'closed'}

enum ScreenIdents {
    MainScreen = "MainScreen",
    ModalScreen = "ModalScreen",
    StackScreen = "StackScreen"
}

interface MainScreen {
    type: ScreenIdents.MainScreen
}

interface ContextIndex {
    index: number,
    type: "ContextIndex"
}

const mkContextIndex = (index: number): ContextIndex => {
    return {index: index, type: "ContextIndex"};
}

interface ContextLabel {
    label: string,
    type: "ContextLabel"
}

const mkContextLabel = (label: string): ContextLabel => {
    return {label: label, type: "ContextLabel"};
}

interface ModalScreen {
    accessor: ContextIndex | ContextLabel,
    type: ScreenIdents.ModalScreen
}

interface StackScreen {
    accessor: ContextIndex | ContextLabel,
    type: ScreenIdents.StackScreen
}

type Screens = MainScreen | ModalScreen | StackScreen;

interface Screen {
    label: string,
    screen: ReactElement
}

interface Stack {
    label: string,
    screens: Screen[]
}

/** The Nav's context. It contains the screens of the Nav. */
export interface NavContext {
    /** The Nav's main screen. */
    mainScreen: ReactElement,
    /** The stacks that can be injected into the Nav. */
    stacks: Stack[]
    /** The modals that can be injected into the Nav. */
    modals: Screen[]
}

const initialNavContext: NavContext = {
    mainScreen: React.createElement("NavScreen"),
    stacks: [],
    modals: []
}

/**
 * Finds the index of the corresponding stack group with identifier `id`.
 * 
 * @param label The identifier of the stack.
 * @returns The `index` for the corresponding stack, or nothing.
 */
const findStack = (context: NavContext, label: string) => {
    return context.stacks.findIndex((value) => {
        return value.label == label
    });
}

/**
 * Finds the index of the corresponding modal with identifier `id`.
 * 
 * @param label The identifier of the modal.
 * @returns The `index` for the corresponding modal, or nothing.
 */
const findModal = (context: NavContext, label: string) => {
    return context.modals.findIndex((value) => {
        return value.label == label
    });
}

export interface NavScreenProps {    
    label: string,
    screen: ReactElement
}

/** 
 * Sets up the context. */
export const NavContext = createContext(initialNavContext);

const extractGroup = (children: ReactNode) => {
    return Children.toArray(children).reduce<Screen[]>((acc, child) => {
      // We have to cast child.type, because NavScreen is a function component.         
      if (React.isValidElement(child) && typeof child.type != 'string') {
        let component: JSXElementConstructor<any> = child.type;
        // Make sure we only have NavScreen's in the group:
        if (component.name == 'NavScreen') {
          // Now the child.props should be a NavScreenProps:
          acc.push(child.props);
          return acc;
        } else {
          throw new Error ('A NavScreen is the only component allowable in a Nav group.')  
        }      
      } else {
        throw new Error ('extractGroup: child is not a valid React element.')
      }    
    }, [])
}

const extractScreens = (main: ReactElement) => (acc:NavContext, child: ReactNode) => {    
    if (React.isValidElement(child) && typeof child.type != 'string') {
        // We have to cast child.type, because NavScreen is a function component.   
        let component: JSXElementConstructor<any> = child.type
        
        // Set the main screen:
        acc.mainScreen = main;

        if (component.name == "NavModals") {
            if (acc.modals.length == 0) {
                acc.modals = extractGroup(child.props.children);
                return acc;
            } else {
                throw new Error ("There are more than one NavModals in the Nav.")
            }
        } else if (component.name == "NavStack") {
            const stackScreens = extractGroup(child.props.children);
            const label = child.props.label;
            acc.stacks.push({label: label, screens: stackScreens});
            return acc;
        } else {
            throw new Error (`A component in NavScreens has the wrong type: ${child.type}.\n 
                NavScreens can only contain MainScreen, NavStack, or NavModals components as children.`)
        }     
    } else {
        throw new Error ('Invalid child found in the Nav.');
    }    
}

/**
 * Get the screens from the children of a NavScreens component.
 * 
 * @param children The children from `NavProps` to extract the screens from.
 * @returns A `NavContext`
 */
export const createNavContext = (main: ReactElement, children: ReactNode) => {
    return Children.toArray(children).reduce<NavContext>(extractScreens(main), initialNavContext);
}

interface NavState {
    /** Used to toggle the Nav from opened to closed. */
    navStatus: NavStatus,    

    /** Determines which screen should be shown in the Nav. */
    currentScreen: Screens,
    
    /**
     * Bimap between stack screen id's and array indices into the Context of
     * stack screens. 
     */
    stackScreenMap: BinTree<string, number>,

    /**
     * Bimap between modal screen id's and array indices into the Context of
     * modal screens. 
     */
    modalScreenMap: BinTree<string, number>,  
}

/** The initial state of the Nav. 
 * 
 * Initially, the Nav starts:
 * - closed, and
 * - showing the `NavGroupType.MainScreen`.
*/
const initialNavState: NavState = {
    /** The Nav always starts out closed. */
    navStatus: navClosed,
    /** The Nav always starts with the `MainScreen`. */
    currentScreen: {type: ScreenIdents.MainScreen},
    /** The stack screen map starts out empty. */
    stackScreenMap: empty(),
    /** The modal screen map starts out empty. */
    modalScreenMap: empty()
}

/**
 * A selector for determining if the Nav is opened.
 * @param state a Redux state containing the `NavState`.
 * @returns `true` if the Nav's status is set to open, and `false` otherwise.
 */
export const isNavOpenedSelector = (state:{nav: NavState}) => {
    return state.nav.navStatus == navOpened
}

const navSlice = createSlice({
    name: 'navStatus',
    initialState: initialNavState,
    reducers: {
        openNav: (state) => {
            state.navStatus = navOpened;
        },
        closeNav: (state) => {
            state.navStatus = navClosed;
        },
        toggleNav: (state) => {
            if (state.navStatus.status == 'opened') {
                state.navStatus = navClosed;
            } else {
                state.navStatus = navOpened;
            }
        },  
        injectMainView: (state) => {
            state.currentScreen = {type: ScreenIdents.MainScreen}
        },
        /**
         * Injects a stack into the Nav.
         * @param payload The id of the stack.
         */
        injectStack: (state, {payload}) => {
            let payloadString: string = payload;
            let stackIndexM: Maybe<number> = lookup(mkKeyString(payloadString), state.stackScreenMap);

            switch(stackIndexM.type) {
                case "just":
                    state.currentScreen = {type: ScreenIdents.StackScreen, accessor: mkContextIndex(stackIndexM.value)};
                case "nothing":
                    return
            }            
        },
        /**
         * Injects a modal into the Nav.
         * @param payload The id of the modal.
         */
        injectModal: (state, {payload}) => {
            let payloadString: string = payload;
            let modalIndexM: Maybe<number> = lookup(mkKeyString(payloadString), state.stackScreenMap);

            switch(modalIndexM.type) {
                case "just":
                    state.currentScreen = {type: ScreenIdents.ModalScreen, accessor: mkContextIndex(modalIndexM.value)};
                case "nothing":
                    return
            }
        },
        /**
         * Link the stack screens in the context with the state.
         * @param payload An array of `Screens`'s containing the stack of screens.
         */
        linkStackScreens: (state, action) => {
            let key: string = action.payload.key;
            let value: number = action.payload.value;

            state.stackScreenMap = insert(mkKeyString(key), value, state.stackScreenMap);

            return state;
        },
        /**
         * Link the modal screens in the context with the state.
         * @param payload A `NavGroupState` containing the modal screens.
         */
        linkModalScreens: (state, action) => {
            let key: string = action.payload.key;
            let value: number = action.payload.value;

            state.modalScreenMap = insert(mkKeyString(key), value, state.modalScreenMap);

            return state;
        }
    },    
});

export const { openNav, closeNav, toggleNav, injectMainView, injectStack, injectModal, linkStackScreens, linkModalScreens } = navSlice.actions;
export const useAppDispatch: () => AppDispatch = useDispatch
export const useAppSelector: TypedUseSelectorHook<RootState> = useSelector

const store = configureStore({ 
    reducer: {
        nav: navSlice.reducer,
    }, 
});

type RootState = ReturnType<typeof store.getState>;
type AppDispatch = typeof store.dispatch;

/*************************************
 *   Hooks for mutating the state.   *
 *************************************/ 

// export const setNavScreen = (context:NavContext, type: NavGroupType, id?: string): number => {
//     // const dispatch = useAppDispatch();    
//     // const setMainView = () => dispatch(injectMainView());
//     // const setStackView = (index: number) => dispatch(injectStack({payload: index}));
//     // const setModalView = (index: number) => dispatch(injectModal({payload: index}));

//     switch (type)  {   
//         case NavGroupType.MainScreen:
//             return 0;

//         case NavGroupType.NavModal:
//             // We should have an id, so use it to get the index of the modal:
//             if (id) {
//                 // Look up the index for the corresponding screen with identifier id:
//                 let indexMaybe = findModal(context, id);
//                 let index = 0;

//                 // Make sure we found it:
//                 switch (indexMaybe.type) {
//                     case "nothing":
//                         throw new Error (`getNavScreen: id = ${id}, but it was not found by findModal.`)        
//                     case "just":
//                         index = indexMaybe.value;
//                 }
//                 // Set the state to the corresponding modal:
//                 return index;
//             } else {
//                 throw new Error ('getNavScreen: id is undefined, but type was set to NavGroupType.NavModal.')
//             }
//         case NavGroupType.NavStack:
//             // We should have an id, so use it to get the index of the modal:
//             if (id) {
//                 // Look up the index for the corresponding screen with identifier id:
//                 let indexMaybe = findStack(context, id);
//                 let index = 0;

//                 // Make sure we found it:
//                 switch (indexMaybe.type) {
//                     case "nothing":
//                         throw new Error (`getNavScreen: id = ${id}, but it was not found by findStack.`)        
//                     case "just":
//                         index = indexMaybe.value;
//                 }
//                 // Set the state to the corresponding stack:
//                 return index;
//             } else {
//                 throw new Error ('getNavScreen: id is undefined, but type was set to NavGroupType.NavStack.')
//             }
//     }
// }

export const currentScreenSelector = (state:RootState) => {
    return state.nav.currentScreen
}

export const getNavScreen = (context: NavContext, state: RootState): ReactNode => {            
    const currentScreen: Screens = currentScreenSelector(state);  

    switch (currentScreen.type) {
        case (ScreenIdents.MainScreen): {
            return context.mainScreen;
        }
        case (ScreenIdents.ModalScreen): {
            let accessor = currentScreen.accessor;
            switch (accessor.type) {
                case ("ContextIndex"): {
                    return context.modals[accessor.index].screen;
                }
                case ("ContextLabel"):  {
                    let mIndex = lookup(mkKeyString(accessor.label), state.nav.modalScreenMap);
                    switch (mIndex.type) {
                        case ("just"):  
                            return context.modals[mIndex.value].screen;
                        case ("nothing"):
                            throw new Error (`getNavScreen: Failed to find the modal with label ${accessor.label}`);
                    }
                }
            }
        }
        case (ScreenIdents.StackScreen): {
            throw new Error ('getNavScreen: TODO StackScreens NOT IMPLEMENTED YET! ')
        }
    }
}

/****************************************************
 *  Components for utilizing the store and context. *
 ****************************************************/

/**
 * The type of props for Nav buttons.
 */
interface NavButtonProps {
    /** The title of the button. */
    title: string;
    /** The call back for when the button is pressed. */
    onPress: (event: GestureResponderEvent) => void;
}

/**
 * A button for navigating to a new screen.
 * @param props - A `NavButtonProps`.
 * @returns A RN Button with the onPress callback wrapped to manage the state of
 * the Nav properly.
 */
export const NaviButton = (props:NavButtonProps) => {
    const dispatch = useAppDispatch();
    const closeTheNav = () => dispatch(closeNav())
    
    const onPressCallback = (event: GestureResponderEvent) => {
        // Make sure the Nav is closed before transitioning to a new screen.
        closeTheNav();
        props.onPress(event);
    }

    return (
        <Button title={props.title} onPress={onPressCallback} />
    )
}

export const NextNavButton = () => {

}

export const GoBackNavButton = () => {

}

export interface NavProviderProps {
    children: ReactNode;
}

/**
 * The Nav Provider.
 */
export default ({children} : NavProviderProps) => {
    return (
        <Provider store={store}>
            {children}            
        </Provider>
    )
}
