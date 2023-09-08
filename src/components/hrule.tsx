import { View } from "react-native";
import { colorTheme } from "../theme/theme";

export const Hrule = () => {
    let hruleHeight = 1;

    return (
        <View style={{backgroundColor: colorTheme.border.primary, width: '100%', height: hruleHeight}}/>
    );
};