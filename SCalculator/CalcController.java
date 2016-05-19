import java.net.URL;
import java.util.ResourceBundle;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.Button;
import javafx.scene.layout.Pane;
import javafx.scene.control.TextField;

public class CalcController extends Pane implements Initializable {
  @FXML private Button btn7;
  @FXML private Button btn8;
  @FXML private TextField txtMain;

  @Override
  public void initialize(URL url, ResourceBundle res) {
    return;
  }

  public void handleKeyPress(ActionEvent event) {
    txtMain.setText(txtMain.getText() + "9");
  }
}